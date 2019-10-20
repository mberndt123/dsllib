libraryDependencies ++= Seq(
  "com.alexknvl"  %%  "polymorphic" % "0.4.0",
  "org.typelevel" %% "cats-core" % "2.0.0"
)
scalaVersion := "2.12.9"
scalacOptions += "-Ypartial-unification"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

