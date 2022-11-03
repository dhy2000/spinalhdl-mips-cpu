ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.13.8"

val spinalVersion = "1.7.3"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

fork := true // to enable SpinalSim

lazy val root = (project in file("."))
  .settings(
    name := "mips-cpu",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin)
  )
