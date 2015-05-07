name := "bendy"

version := "0.1"

scalaVersion := "2.11.6"

description := "Extensible Protocols for Scala"

licenses += ("BSD Simplified", url("https://github.com/ermine-language/ermine-legacy/blob/master/LICENSE"))

seq(bintraySettings:_*)

bintray.Keys.bintrayOrganization in bintray.Keys.bintray := Some("lastik")

resolvers += "tpolecat"  at "http://dl.bintray.com/tpolecat/maven"

resolvers += "runarorama"  at "http://dl.bintray.com/runarorama/maven"

publishMavenStyle := true

scalacOptions ++=
  Seq("-encoding", "UTF-8", "-Yrecursion", "50", "-deprecation",
      "-unchecked", "-Xlint", "-feature",
      "-language:implicitConversions", "-language:higherKinds",
      "-language:existentials")

javacOptions ++=
  Seq("-Xlint:cast", "-Xlint:deprecation", "-Xlint:empty",
      "-Xlint:finally", "-Xlint:fallthrough", "-Xlint:overrides")

javaOptions ++= Seq("-XX:MaxJavaStackTraceDepth=1000000 -Dscala.color")

parallelExecution := true

javacOptions += "-Xlint"

scalacOptions ~= (so => (so filterNot Set("-unchecked", "-Xlint"))
                    ++ Seq("-Ywarn-nullary-override", "-Ywarn-inaccessible"))

libraryDependencies ++= Seq("org.scalaz" %% "scalaz-core" % "7.1.+",
                           "org.tpolecat" %% "atto-core" % "0.4.2.1r",
                           "bound" %% "bound-core" % "1.3.0")

libraryDependencies ++= Seq("org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
                            "bound"          %% "bound-scalacheck-binding"  % "1.3.0" % "test")
