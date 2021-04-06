import mill._, scalalib._

object deobfuscator extends ScalaModule {
  def scalaVersion = "3.0.0-RC1"
  def scalacOptions = ScalacOptions()

  def ivyDeps = Agg(
    ivy"org.ow2.asm:asm::9.1",
    ivy"org.ow2.asm:asm-commons::9.1",
    ivy"org.typelevel::cats-effect::3.0.1",
    ivy"co.fs2::fs2-core::3.0.1"
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
