import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._

object deobfuscator extends ScalaModule with ScalafmtModule {
  def scalaVersion = "3.0.0"
  def scalacOptions = ScalacOptions()

  def ivyDeps = Agg(
    ivy"org.ow2.asm:asm:9.1",
    ivy"org.ow2.asm:asm-commons:9.1",
    ivy"org.typelevel::cats-effect:3.1.1",
    ivy"co.fs2::fs2-core:3.0.3",
    ivy"io.bullet:borer-core_2.13:1.7.1"
  )
}

object ScalacOptions {
  def apply() =
    Seq(
      "-feature",
      "-unchecked",
      "-deprecation",
      "-language:implicitConversions",
      "-Xfatal-warnings"
    )
}
