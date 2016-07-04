
import sbt._

name := "Sudoku"

version := "0.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalatest"   %% "scalatest"   % "2.2.4"  % "test"
)

