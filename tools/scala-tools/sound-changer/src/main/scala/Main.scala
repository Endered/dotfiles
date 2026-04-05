import scala.sys.process.given
import layoutz.Cmd
import layoutz.Sub

val extractVolumeRegex = "(\\d+)%".r

case class SoundState(volume: Int, mute: Boolean)

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

def getSoundState(): SoundState = {
  SoundState(
    volume = getAverageVolume().getOrElse(0),
    mute = getIsMute()
  )
}

def changeVolume(diff: Int): Unit = {
  val signedDiff = if (diff >= 0) {
    s"+${diff}"
  } else {
    s"-${-diff}"
  }

  s"pactl set-sink-volume @DEFAULT_SINK@ ${signedDiff}%".!
}

def changeMute(): Unit = {
  "pactl set-sink-mute @DEFAULT_SINK@ toggle".!
}

object SoundChangerApp extends layoutz.LayoutzApp[SoundState, String] {
  import layoutz.*

  override def init: (SoundState, Cmd[String]) = getSoundState()

  override def update(
      msg: String,
      state: SoundState
  ): (SoundState, Cmd[String]) = {
    msg match {
      case "up" =>
        changeVolume(+1)
        state.copy(volume = state.volume + 1)
      case "down" =>
        changeVolume(-1)
        state.copy(volume = 0.max(state.volume - 1))
      case "mute" =>
        changeMute()
        state.copy(mute = !state.mute)
      case "tick" => getSoundState()
      case _      => state
    }
  }

  override def subscriptions(state: SoundState): Sub[String] =
    Sub.batch(
      Sub.time.everyMs(1000, "tick"),
      Sub.onKeyPress {
        case Key.Char('k') => Some("up")
        case Key.Char('j') => Some("down")
        case Key.Char('m') => Some("mute")
        case _             => None
      }
    )
  override def view(state: SoundState): Element = {
    val barLabel = if (state.mute) {
      "Volume(muted)"
    } else {
      "Volume"
    }

    inlineBar(barLabel, state.volume.toDouble / 100)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    SoundChangerApp.run(
      quitKey = layoutz.Key.Char('q')
    )
  }
}
