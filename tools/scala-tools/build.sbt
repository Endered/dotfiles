import bindgen.interface.Binding
import bindgen.plugin.BindgenMode

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.mutable.ListBuffer
import scala.scalanative.build.*
import scala.sys.process
import scala.sys.process.stringToProcess
import scala.util.Try
import scala.util.chaining.scalaUtilChainingOps

lazy val commonSettings = Seq(
  scalaVersion := "3.8.3",
  nativeConfig ~= {
    _.withLTO(LTO.thin)
      .withMode(Mode.releaseFast)
      .withGC(GC.boehm)
      .withIncrementalCompilation(true)
  },
)

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    commonSettings,
    nativeConfig ~= {
      _.withCompileOptions(_ ++ gioCflags)
        .withLinkingOptions(_ ++ pkgConfig("gio-2.0", "--libs"))
    },
  )
  .dependsOn(
    cpuHealz,
    soundChanger,
    bluetoothBatteryWatcher,
  )

lazy val cpuHealz = project
  .in(file("cpu-healz"))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    name := "cpu-healz",
    commonSettings,
  )

lazy val soundChanger = project
  .in(file("sound-changer"))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    name := "sound-changer",
    commonSettings,
    libraryDependencies += "xyz.matthieucourt" %%% "layoutz" % "0.7.0",
  )

lazy val bluetoothBatteryWatcher = project
  .in(file("bluetooth-battery-watcher"))
  .enablePlugins(ScalaNativePlugin, BindgenPlugin)
  .settings(
    name := "bluetoothBatteryWatcher",
    commonSettings,
    update / aggregate := false,
    nativeConfig ~= {
      _.withCompileOptions(_ ++ gioCflags)
        .withLinkingOptions(_ ++ pkgConfig("gio-2.0", "--libs"))
    },
  )
  .dependsOn(libgio)

lazy val libgio = project
  .in(file("libgio"))
  .enablePlugins(ScalaNativePlugin, BindgenPlugin)
  .settings(
    name := "libgio",
    commonSettings,
    bindgenBindings := Seq(
      Binding(
        (Compile / resourceDirectory).value / "scala-native" / "header.h",
        "libgio",
      ).addCImport("gio/gio.h")
        .addClangFlag(gioCflags)
        .withMultiFile(true)
        .withOpaqueStructs(Set("G*"))
        .pipe(gioIncludePath.foldLeft(_)(_.addExcludedSystemPath(_))),
    ),
    nativeConfig ~= {
      _.withCompileOptions(_ ++ gioCflags)
        .withLinkingOptions(_ ++ pkgConfig("gio-2.0", "--libs"))
    },
    trackInternalDependencies := TrackLevel.TrackIfMissing,
    bindgenBinary := {
      val existing = bindgenBinary.value
      sys.env
        .get("BINDGEN_PATH")
        .map(path => Paths.get(path).toFile())
        .getOrElse(existing)
    },
  )

// Seq("-I/hoge/fuga", ...)
lazy val gioCflags = pkgConfig("gio-2.0", "--cflags")
// Seq("/hoge/fuga", ...)
lazy val gioIncludePath =
  gioCflags.filter(_.startsWith("-I")).map(_.drop(2)).map(Paths.get(_))

def pkgConfig(pkgName: String, option: String): Seq[String] = {
  val buffer = ListBuffer.empty[String]
  process
    .Process("pkg-config", Seq(pkgName, option))
    .!(process.ProcessLogger(buffer += _, _ => ()))

  buffer.flatMap(_.split(" ")).filter(_ != "").toSeq
}

def findHeader(candidatePaths: Seq[Path], pathGenerator: Path => Path): Path = {
  candidatePaths.toIterator.map(pathGenerator(_)).find(Files.exists(_)).get
}
