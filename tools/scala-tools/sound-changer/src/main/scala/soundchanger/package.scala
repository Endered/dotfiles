package soundchanger

import scala.sys.process.given
import layoutz.Cmd
import layoutz.Sub
import io.circe.Decoder
import io.circe.parser.decode
import scala.util.chaining.given

val extractVolumeRegex = "(\\d+)%".r

case class SoundState(
    volume: Int,
    mute: Boolean,
    devices: Seq[Device],
    cursor: Int,
) {
  def selectedDevice: Device = devices(cursor)
}

case class Volume(
    `value_percent`: String,
) derives Decoder {
  def value: Int = `value_percent` match {
    case s"${v}%" =>
      v.toInt
  }
}
case class Volumes(
    `front-left`: Volume,
    `front-right`: Volume,
) derives Decoder {
  def averageVolume = {
    (`front-left`.value + `front-right`.value) / 2
  }
}
case class Device(
    name: String,
    description: String,
    mute: Boolean,
    state: String,
    volume: Volumes,
) derives Decoder {
  def averageVolume = volume.averageVolume
  def isSelected = state == "RUNNING"
}

def getAverageVolume(): Option[Int] = {
  val output = "pactl get-sink-volume @DEFAULT_SINK@".!!
  val volumes =
    extractVolumeRegex.findAllMatchIn(output).map(_.group(1).toInt).toSeq

  if (volumes.isEmpty) {
    None
  } else {
    Some(volumes.sum / volumes.length)
  }
}

def getIsMute(): Boolean = {
  "pactl get-sink-mute @DEFAULT_SINK@".!!.contains("yes")
}

def getSoundState(cursor: Int): SoundState = {
  val devices = getDeviceNames()
  SoundState(
    volume = getAverageVolume().getOrElse(0),
    mute = getIsMute(),
    devices = getDeviceNames(),
    cursor = cursor,
  )
}

def changeVolume(name: String, diff: Int): Unit = {
  val signedDiff = if (diff >= 0) {
    s"+${diff}"
  } else {
    s"-${-diff}"
  }

  s"pactl set-sink-volume ${name} ${signedDiff}%".!
}

def changeMute(name: String): Unit = {
  s"pactl set-sink-mute ${name} toggle".!
}

def getDeviceNames(): Seq[Device] = {
  val res = "pactl -f json list sinks".!!
  val parsed = decode[Seq[Device]](res)
  parsed.right.get.sortBy(_.description)
}

def renderWhen(render: Boolean)(text: String) = {
  if (render) {
    text
  } else {
    " " * text.size
  }
}

def changeDefaultSink(name: String) = {
  s"pactl set-default-sink ${name}".!
}

object SoundChangerApp extends layoutz.LayoutzApp[SoundState, String] {
  import layoutz.*

  override def init: (SoundState, Cmd[String]) = getSoundState(0)

  override def update(
      msg: String,
      state: SoundState,
  ): (SoundState, Cmd[String]) = {
    msg
      .match {
        case "right" =>
          changeVolume(state.selectedDevice.name, +1)
          getSoundState(state.cursor)
        case "left" =>
          changeVolume(state.selectedDevice.name, -1)
          getSoundState(state.cursor)
        case "up" =>
          state.copy(cursor = state.cursor - 1)
        case "down" =>
          state.copy(cursor = state.cursor + 1)
        case "mute" =>
          changeMute(state.selectedDevice.name)
          getSoundState(state.cursor)
        case "change" =>
          changeDefaultSink(state.selectedDevice.name)
          getSoundState(state.cursor)
        case "tick" => getSoundState(state.cursor)
        case _      => state
      }
      .pipe { s =>
        s.copy(
          cursor = Math.clamp(s.cursor, 0, s.devices.size - 1),
        )
      }
  }

  override def subscriptions(state: SoundState): Sub[String] =
    Sub.batch(
      Sub.time.everyMs(1000, "tick"),
      Sub.onKeyPress {
        case Key.Char('k') => Some("up")
        case Key.Char('j') => Some("down")
        case Key.Char('h') => Some("left")
        case Key.Char('l') => Some("right")
        case Key.Char('m') => Some("mute")
        case Key.Char('c') => Some("change")
        case _             => None
      },
    )
  override def view(state: SoundState): Element = {

    def renderDevice(device: Device, cursor: Boolean) = {
      columns(
        renderWhen(cursor)(" => "),
        "[" + renderWhen(device.isSelected)("X") + "]: ",
        inlineBar(
          if (device.mute) " Mute " else "Unmute",
          device.averageVolume.toDouble / 100,
        ),
        device.description,
      )
    }

    layout(
      state.devices.zipWithIndex
        .map((device, index) => renderDevice(device, state.cursor == index))*,
    )
  }
}

def run(args: Array[String]): Unit = {
  SoundChangerApp.run(
    quitKey = layoutz.Key.Char('q'),
  )
}
