import scala.scalanative.build._

scalaVersion := "3.6.3"
name := "cpu-healz"

enablePlugins(ScalaNativePlugin)

nativeConfig ~= {
  _.withLTO(LTO.thin)
    .withMode(Mode.releaseFast)
    .withGC(GC.boehm)
}
