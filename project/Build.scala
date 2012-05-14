import sbt._
import Keys._
import sbtassembly.Plugin._

/**
 * @author eiennohito
 * @since 14.05.12
 */
object UtfCheckerBbuild extends Build {
  lazy val project = Project("checker", file("."), settings = Project.defaultSettings ++ assemblySettings)
}

