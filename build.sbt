
lazy val baseSettings: Seq[Setting[_]] = Seq(
  scalaVersion       := "2.12.0",
  scalacOptions     ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions", "-language:existentials",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"
  ),
  resolvers += Resolver.sonatypeRepo("releases")
)

lazy val fpSudoku = project.in(file("."))
  .settings(moduleName := "FPSudoku")
  .settings(baseSettings: _*)
  .aggregate(core, slides)
  .dependsOn(core, slides)

lazy val core = project
  .settings(moduleName := "FPSudoku-core")
  .settings(baseSettings: _*)
  .settings(libraryDependencies ++=Seq("org.scalatest"   %% "scalatest"   % "3.0.1"  % "test"))


lazy val slides = project
  .settings(moduleName := "FPSudoku-slides")
  .settings(baseSettings: _*)
  .settings(tutSettings: _*)
  .settings(
    tutSourceDirectory := baseDirectory.value / "tut",
    tutTargetDirectory := baseDirectory.value / "tut-out"
  ).dependsOn(core)

initialCommands in console := """// helper method to disable type printing
def shortresults[T](t: => T) = {
   val s = t.toString
   val name = s.takeWhile(_ != ':')
   val idx = s.indexOf(" = ")
   val full = if (idx >= 0) name + s.substring(idx) else s
   val short = if (full.length>799) full.substring(0,796)+"..." else full
   print(short)
   t
}
"""