// -*- mode: scala -*-

import mill._
import mill.api.Loose
import mill.scalalib._
import mill.scalalib.publish._
import coursier.MavenRepository

import scala.util.Properties

object meta {

  val crossVersions = Seq("2.13.6")

  implicit val wd: os.Path = os.pwd

  def nonEmpty(s: String): Option[String] = s.trim match {
    case v if v.isEmpty => None
    case v              => Some(v)
  }

  val MILL_VERSION = Properties.propOrNull("MILL_VERSION")
  val versionFromEnv = Properties.propOrNone("PUBLISH_VERSION")
  val gitSha = nonEmpty(os.proc("git", "rev-parse", "--short", "HEAD").call().out.trim)
  val gitTag = nonEmpty(
    os.proc("git", "tag", "-l", "-n0", "--points-at", "HEAD").call().out.trim
  )
  val publishVersion =
    (versionFromEnv orElse gitTag orElse gitSha).getOrElse("latest")
}

object scalable extends Cross[Scalable](meta.crossVersions: _*)
class Scalable(val crossScalaVersion: String)
    extends CrossScalaModule
    with PublishModule { self =>
  def publishVersion = meta.publishVersion

  override def artifactName = "scalable"

  def pomSettings = PomSettings(
    description = "Scalable Algebraic Effects",
    organization = "com.github.vic",
    url = "https://github.com/vic/scalable",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("vic", "scalable"),
    developers = Seq(
      Developer("vic", "Victor Borja", "https://github.com/vic")
    )
  )

  override def ivyDeps: T[Loose.Agg[Dep]] =
    Agg(ivy"com.github.vic:typeset:98708fc") ++ super.ivyDeps()

  override def repositoriesTask = T.task {
    Seq(MavenRepository("https://jitpack.io")) ++ super.repositoriesTask()
  }

  object tests extends Tests {
    override def ivyDeps =
      Agg(ivy"com.lihaoyi::utest::0.7.4") ++ self.compileIvyDeps()
    override def testFrameworks = Seq("utest.runner.Framework")
  }
}
