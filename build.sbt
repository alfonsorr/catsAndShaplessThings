name := "shapelessTest"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.typelevel" %% "cats-core" % "1.0.0-RC2"
)

//scalacOptions ++= Seq("-Ypartial-unification")