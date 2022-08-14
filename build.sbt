/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2022 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

ThisBuild/organization := "comp.mq.edu.au"
ThisBuild/scalaVersion := "2.12.8"
ThisBuild/scalacOptions :=
    Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Xcheckinit",
//        "-Xfatal-warnings",
//        "-Xlint:-stars-align,_"
    )
ThisBuild/resolvers += Resolver.bintrayRepo("underscoreio", "training")

// Settings for the assignment 1 project.
lazy val ass1 = (project in file("."))
  .settings(
    // Project information
    name := "Comp3000 interpret",
    version := "0.1",

    // Fork the JVM to run the doodle application. Then when the application
    // exits it won't kill the JVM that is running SBT.
    fork in run := true,

    // include API mappings
    autoAPIMappings := true,

    // Dependencies
    libraryDependencies ++=
      Seq (
        "junit" % "junit" % "4.12" % "test",
        "org.scalatest" %% "scalatest" % "3.0.8" % "test",
        "org.creativescala" %% "doodle" % "0.9.4"
      )
  )

// Interactive settings

connectInput in run := true

logLevel := Level.Info

shellPrompt := {
    state =>
        Project.extract(state).currentRef.project + " " + version.value +
            " " + scalaVersion.value + "> "
}

// send output to the build's standard output and error to avoid
// truncation of the number of lines that are output
outputStrategy := Some(StdoutOutput)

