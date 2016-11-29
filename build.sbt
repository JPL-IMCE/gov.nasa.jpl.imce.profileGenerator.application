import java.io.File

import sbt.Keys._
import sbt._
import spray.json._
import DefaultJsonProtocol._
import com.typesafe.sbt.pgp.PgpKeys
import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._
import scala.xml._

enablePlugins(AetherPlugin)

useGpg := true

fork in run := true

updateOptions := updateOptions.value.withCachedResolution(true)

resolvers += 
"Artifactory" at "https://cae-artifactory.jpl.nasa.gov/artifactory/ext-release-local/"

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

lazy val mdRoot = SettingKey[File]("md-root", "MagicDraw Installation Directory")

lazy val specsRoot = SettingKey[File]("specs-root", "MagicDraw DynamicScripts Test Specification Directory")

lazy val runMDTests = taskKey[Unit]("Run MagicDraw DynamicScripts Unit Tests")

lazy val artifactZipFile = taskKey[File]("Location of the zip artifact file")

lazy val zipInstall = TaskKey[File]("zip-install", "Zip the resources")

lazy val core = Project("gov-nasa-jpl-imce-profileGenerator", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(dynamicScriptsResourceSettings("gov.nasa.jpl.imce.profileGenerator"))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
  .dependsOnSourceProjectOrLibraryArtifacts(
    "gov-nasa-jpl-imce-profileGenerator-model-bundle",
    "gov.nasa.jpl.imce.profileGenerator.model.bundle",
    Seq(
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.imce.profileGenerator.model.bundle"
        % Versions_profileGenerator_model_bundle.version %
        "compile" withSources() withJavadoc() artifacts
        Artifact( "gov.nasa.jpl.imce.profileGenerator.model.bundle", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "gov-nasa-jpl-imce-profileGenerator-model-profile",
    "gov.nasa.jpl.imce.profileGenerator.model.profile",
    Seq(
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.imce.profileGenerator.model.profile"
        % Versions_profileGenerator_model_profile.version %
        "compile" withSources() withJavadoc() artifacts
        Artifact( "gov.nasa.jpl.imce.profileGenerator.model.profile", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "imce-magicdraw-dynamicscripts-batch",
    "imce.magicdraw.dynamicscripts.batch",
    Seq(
      "org.omg.tiwg" %% "imce.magicdraw.dynamicscripts.batch"
        % Versions_imce_magicdraw_dynamicScripts_batch.version artifacts
        Artifact("imce.magicdraw.dynamicscripts.batch", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "imce-magicdraw-dynamicscripts-batch",
    "imce.magicdraw.dynamicscripts.batch",
    Seq(
      "org.omg.tiwg" %% "imce.magicdraw.dynamicscripts.batch"
      % Versions_imce_magicdraw_dynamicScripts_batch.version
      % "test"
      classifier "tests"
      artifacts
        Artifact("imce.magicdraw.dynamicscripts.batch", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "projectUsageIntegrityChecker",
    "gov.nasa.jpl.magicdraw.projectUsageIntegrityChecker",
    Seq(
      "gov.nasa.jpl.imce" % "gov.nasa.jpl.magicdraw.projectUsageIntegrityChecker"
        % Versions_puic.version % "compile"
        withSources() withJavadoc() artifacts
        Artifact("gov.nasa.jpl.magicdraw.projectUsageIntegrityChecker", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "com-nomagic-magicdraw-package",
    "com.nomagic.magicdraw.package",
    Seq(
      "org.omg.tiwg.vendor.nomagic" % "com.nomagic.magicdraw.package"
        % "18.0-sp6" % "compile"
        artifacts
        Artifact("com.nomagic.magicdraw.package", "pom", "pom", None, Seq(), None, Map())
    )
  )
  .settings(
    IMCEKeys.licenseYearOrRange := "2016",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,
    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,

    buildInfoPackage := "gov.nasa.jpl.imce.profileGenerator",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "generic.library")
    },

    //libraryDependencies += "org.omg.tiwg.vendor.nomagic" % "com.nomagic.magicdraw.package" % "18.0-sp6",

    resourceDirectory in Compile :=
      baseDirectory.value / "resources",

    scalaSource in Test :=
      baseDirectory.value / "src" / "test" / "scala",

    resourceDirectory in Test :=
      baseDirectory.value / "resources",

    // disable publishing the jar produced by `test:package`
    publishArtifact in(Test, packageBin) := false,

    // disable publishing the test API jar
    publishArtifact in(Test, packageDoc) := false,

    // disable publishing the test sources jar
    publishArtifact in(Test, packageSrc) := false,

    unmanagedClasspath in Compile ++= (unmanagedJars in Compile).value,

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg"),
    resolvers += Resolver.bintrayRepo("tiwg-vendor", "org.omg.tiwg.vendor.nomagic"),

    extractArchives := {
      val base = baseDirectory.value
      val up = update.value
      val s = streams.value
      val sbv = scalaBinaryVersion.value

      val mdInstallDir = base / "target" / "md.package"
      if (!mdInstallDir.exists) {

        val libDir = mdInstallDir / "lib"

        IO.createDirectory(mdInstallDir)

        val tfilter: DependencyFilter = new DependencyFilter {
          def apply(c: String, m: ModuleID, a: Artifact): Boolean =
            (a.extension == "pom" &&
              m.organization.startsWith("org.omg.tiwg.vendor.nomagic") &&
                m.name.startsWith("com.nomagic.magicdraw.package"))
        }
        // https://github.com/sbt/sbt/issues/2461
        /*up.allModules.foreach { mod =>
          if (mod.extraAttributes.get("md.core") != None) {
            s.log.info(
              s"=> (Modules) NEED TO DOWNLOAD FROM ${mod.extraAttributes.get("md.core")}"
            )
          }
          else
            s.log.info(
              s"=> (BABOOM) ${mod.extraDependencyAttributes} ${mod}"
            )
        }
        up.allModules.foreach { mod =>
          mod.explicitArtifacts.foreach { art =>
            if (art.extraAttributes.get("md.core") != None) {
              s.log.info(
                s"=> NEED TO DOWNLOAD FROM ${art.extraAttributes.get("md.core")}"
              )
            }
            else
              s.log.info(
                s"=> (BABOOM ART) ${art.extraAttributes}"
              )
          }
        }*/
        val ts: Seq[File] = up.matching(tfilter)
        ts.foreach { pom =>
          // Use unzipURL to download & extract
          //val files = IO.unzip(zip, mdInstallDir)
          val mdNoInstallZipDownloadURL = ((XML.load(pom.absolutePath) \\ "properties") \ "md.core").text

          s.log.info(
            s"=> found: ${pom.getName} at ${mdNoInstallZipDownloadURL}")
        }

        val pfilter: DependencyFilter = new DependencyFilter {
          def apply(c: String, m: ModuleID, a: Artifact): Boolean =
            (a.`type` == "zip" || a.`type` == "resource") &&
              a.extension == "zip" &&
              (m.organization.startsWith("gov.nasa.jpl") || m.organization.startsWith("com.nomagic")) &&
              (m.name.startsWith("cae_md") ||
                m.name.startsWith("gov.nasa.jpl.magicdraw.projectUsageIntegrityChecker") ||
                m.name.startsWith("imce.dynamic_scripts.magicdraw.plugin") ||
                m.name.startsWith("com.nomagic.magicdraw.package"))
        }
        val ps: Seq[File] = up.matching(pfilter)
        ps.foreach { zip =>
          // Use unzipURL to download & extract
          val files = IO.unzip(zip, mdInstallDir)
          s.log.info(
            s"=> created md.install.dir=$mdInstallDir with ${files.size} " +
              s"files extracted from zip: ${zip.getName}")
        }

        val mdDynamicScriptsDir = mdInstallDir / "dynamicScripts"
        IO.createDirectory(mdDynamicScriptsDir)

        val zfilter: DependencyFilter = new DependencyFilter {
          def apply(c: String, m: ModuleID, a: Artifact): Boolean =
            (a.`type` == "zip" || a.`type` == "resource") &&
              a.extension == "zip" &&
              (m.organization.startsWith("gov.nasa.jpl") || m.organization.startsWith("org.omg.tiwg")) &&
              !(m.name.startsWith("cae_md") ||
                m.name.startsWith("gov.nasa.jpl.magicdraw.projectUsageIntegrityChecker") ||
                m.name.startsWith("imce.dynamic_scripts.magicdraw.plugin") ||
                m.name.startsWith("imce.third_party"))
        }
        val zs: Seq[File] = up.matching(zfilter)
        zs.foreach { zip =>
          val files = IO.unzip(zip, mdDynamicScriptsDir)
          s.log.info(
            s"=> extracted ${files.size} DynamicScripts files from zip: ${zip.getName}")
        }

      } else
        s.log.info(
          s"=> use existing md.install.dir=$mdInstallDir")
    },

    unmanagedJars in Compile := {
      val prev = (unmanagedJars in Compile).value
      val base = baseDirectory.value
      val s = streams.value
      val _ = extractArchives.value

      val mdInstallDir = base / "target" / "md.package"

      val depJars = ((base / "lib") ** "*.jar").get.map(Attributed.blank)

      val libJars = (mdInstallDir ** "*.jar").get.map(Attributed.blank)
      val allJars = libJars ++ depJars

      s.log.info(s"=> Adding ${allJars.size} unmanaged jars")

      allJars
    },

    unmanagedJars in Test := (unmanagedJars in Compile).value,

    compile in Compile := (compile in Compile).dependsOn(extractArchives).value

  )

def dynamicScriptsResourceSettings(projectName: String): Seq[Setting[_]] = {

  import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._

  def addIfExists(f: File, name: String): Seq[(File, String)] =
    if (!f.exists) Seq()
    else Seq((f, name))

  val QUALIFIED_NAME = "^[a-zA-Z][\\w_]*(\\.[a-zA-Z][\\w_]*)*$".r

  Seq(
    // the '*-resource.zip' archive will start from: 'dynamicScripts'
    com.typesafe.sbt.packager.Keys.topLevelDirectory in Universal := None,

    // name the '*-resource.zip' in the same way as other artifacts
    com.typesafe.sbt.packager.Keys.packageName in Universal :=
      normalizedName.value + "_" + scalaBinaryVersion.value + "-" + version.value + "-resource",

    // contents of the '*-resource.zip' to be produced by 'universal:packageBin'
    mappings in Universal in packageBin ++= {
      val dir = baseDirectory.value
      val bin = (packageBin in Compile).value
      val src = (packageSrc in Compile).value
      val doc = (packageDoc in Compile).value
      val binT = (packageBin in Test).value
      val srcT = (packageSrc in Test).value
      val docT = (packageDoc in Test).value

      (dir * "*.md").pair(rebase(dir, projectName)) ++
        (dir / "profiles" ** "*.mdzip").pair(rebase(dir, "..")) ++
        (dir / "modelLibraries" ** "*.mdzip").pair(rebase(dir, "..")) ++
        (dir / "resources" ***).pair(rebase(dir, projectName)) ++
        addIfExists(bin, projectName + "/lib/" + bin.name) ++
        addIfExists(binT, projectName + "/lib/" + binT.name) ++
        addIfExists(src, projectName + "/lib.sources/" + src.name) ++
        addIfExists(srcT, projectName + "/lib.sources/" + srcT.name) ++
        addIfExists(doc, projectName + "/lib.javadoc/" + doc.name) ++
        addIfExists(docT, projectName + "/lib.javadoc/" + docT.name)
    },

    artifacts += {
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map())
    },

    packagedArtifacts += {
      val p = (packageBin in Universal).value
      val n = (name in Universal).value

      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    }
  )
}