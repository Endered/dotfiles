import scala.scalanative.build._

lazy val root = project
  .in(file("."))
  .aggregate(cpuHealz)

lazy val cpuHealz = project
  .in(file("cpu-healz"))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    name := "cpu-healz",
    scalaVersion := "3.8.3",
    nativeConfig ~= {
      _.withLTO(LTO.thin)
        .withMode(Mode.releaseFast)
        .withGC(GC.boehm)
    }
  )
