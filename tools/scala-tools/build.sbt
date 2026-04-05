import scala.scalanative.build._

lazy val commonSettings = Seq(
  scalaVersion := "3.8.3",
  nativeConfig ~= {
    _.withLTO(LTO.thin)
      .withMode(Mode.releaseFast)
      .withGC(GC.boehm)
  }
)

lazy val root = project
  .in(file("."))
  .aggregate(
    cpuHealz,
    soundChanger
  )

lazy val cpuHealz = project
  .in(file("cpu-healz"))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    name := "cpu-healz",
    commonSettings
  )

lazy val soundChanger = project
  .in(file("sound-changer"))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    name := "sound-changer",
    commonSettings,
    libraryDependencies += "xyz.matthieucourt" %%% "layoutz" % "0.7.0"
  )
