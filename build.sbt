libraryDependencies ++= Seq(
  "com.alexknvl"  %%  "polymorphic" % "0.4.0",
  "org.typelevel" %% "cats-core" % "2.0.0"
)
scalaVersion := "2.13.1"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

