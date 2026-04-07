import scala.scalanative.runtime
import java.nio.file.Paths

object Main {
  def main(args: Array[String]): Unit = {
    val executableName = runtime.filename
    val executablePath = Paths.get(executableName)
    val basename = executablePath.getFileName().toString()

    basename match {
      case "my-cpu-measure-tool" => cpuhealz.run(args)
      case "sound-changer"       => soundchanger.run(args)
      case _ => throw new Error(s"Unknown executable file name ${basename}")
    }
  }
}
