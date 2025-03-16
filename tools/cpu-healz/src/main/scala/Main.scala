import scala.util.Try
import java.io.File

val baseDirPath = "/sys/devices/system/cpu/cpufreq/"

object Main {

  def getCPUHealzs(): Option[Seq[Double]] = {
    Try {
      (new File(baseDirPath))
        .list()
        .map(s =>
          scala.io.Source.fromFile(baseDirPath + s + "/scaling_cur_freq")
        )
        .map { s =>
          val clock = s.mkString
          s.close()
          clock
        }
        .map(_.toDouble / 1000)
        .toSeq
    }.toOption
  }

  def ensureLength(n: Int)(str: String): String = {
    val len = Math.max(n - str.length(), 0)
    " " * len + str
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      Thread.sleep(1000)
      for {
        healzs <- getCPUHealzs()
        avgHealz <- Option
          .when(!healzs.isEmpty)(healzs.sum / healzs.length)
          .map(_.toInt)
      } {
        val avgString = ensureLength(9)(s"${avgHealz}MHz")
        println(avgString)
      }
    }
  }
}
