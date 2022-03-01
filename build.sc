import os.Path
import $file.benchmark
import $file.publishable
import $file.platform
import $file.variadic
import mill._, scalalib._
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:`
import mill.contrib.buildinfo.BuildInfo

import com.github.lolgab.mill.mima._

object v {
   val munit = "1.0.0-M1"
   val jmh = "1.33"
   val jnr = "2.2.3"
   val jna = "5.9.0"
}

object slinc
    extends ScalaModule
    with platform.PlatformTypegen
    with publishable.PublishableModule
    with benchmark.BenchmarksModule
    with variadic.VariadicGen {

   def moduleDeps = Seq(polymorphics)
   def ivyDeps = Agg(
     ivy"org.typelevel::cats-core:2.7.0",
     ivy"io.gitlab.mhammons::ffi-j17:0.1.1",
     ivy"io.gitlab.mhammons::ffi-j18:0.1.1")
   def scalaVersion = "3.1.1"
   def pomSettings = pomTemplate("SLinC - Scala <-> C Interop")

   def scalacOptions = Seq(
     "-deprecation",
     "-Wunused:all",
     "-unchecked",
     "-Xcheck-macros",
     "-Xprint-suspension"
   )

   def scalaDocOptions = T {
      super.scalaDocOptions() ++ Seq(
        "-project-logo",
        (millSourcePath / "docs" / "images" / "logo.svg").toString,
        "-social-links:github::https://github.com/markehammons/SLInC,custom::https://gitlab.com/mhammons/slinc::gitlab-white.svg::gitlab-black.svg"
      )
   }

   def forkArgs = Seq(
     "--add-modules",
     "jdk.incubator.foreign",
     "--enable-native-access",
     "ALL-UNNAMED"
   )

   def docSources = T.sources {
      super.docSources() :+ PathRef(millSourcePath / "docs")
   }
   object test extends Tests with TestModule.Munit with BuildInfo {
      def scalacOptions = Seq(
        "-deprecation",
        "-Wunused:all",
        "-unchecked",
        "-Xcheck-macros",
        "-Xprint-suspension",
        "-Xsemanticdb",
        "-Ydump-sbt-inc"
      )

      def forkArgs = Seq(
        "--add-modules",
        "jdk.incubator.foreign",
        "--enable-native-access",
        "ALL-UNNAMED"
      )

      def ivyDeps = Agg(ivy"org.scalameta::munit::${v.munit}")
      def nativeSource = T.sources { millSourcePath / "native" }
      def buildInfoMembers = T {
         compileNative()
            .map(p => p.path.last.stripSuffix(".so") -> p.path.toString())
            .toMap
      }

      def buildInfoPackageName = Some("io.gitlab.mhammons.slinc")
      def compileNative = T {
         val nativeFiles = nativeSource().head.path
         val cFiles = os.list(nativeFiles).filter(_.last.endsWith(".c"))
         cFiles
            .flatMap { p =>
               val soLocation =
                  T.dest / s"lib${p.last.stripSuffix(".c")}.so"
               os.proc(
                 "gcc",
                 "-shared",
                 "-fPIC",
                 "-Wall",
                 "-o",
                 T.dest / s"lib${p.last.stripSuffix(".c")}.so",
                 p
               ).call()
               List(PathRef(soLocation))
            }

      }

      override def compile = T {
         compileNative()
         super.compile()
      }
   }

   object bench extends Benchmarks {
      def jmhVersion = "1.33"
   }
}

object `slinc-ffi` extends ScalaModule {
   def scalaVersion = "3.1.1"
   override def sources = {
      System.getProperty("java.version") match {
         case s"17$x" => T.sources{millSourcePath / "src-17"}
      }
   }
}

object `slinc-ffi-api` extends ScalaModule {
   def scalaVersion = "3.1.1"
}

object cstd extends ScalaModule with benchmark.BenchmarksModule {
   def scalaVersion = "3.1.1"
   def moduleDeps = Seq(slinc)

   object test extends Tests with TestModule.Munit {
      def scalacOptions = Seq(
        "-deprecation",
        "-Wunused:all",
        "-unchecked",
        "-Xcheck-macros",
        "-Xprint-suspension",
        "-Xsemanticdb"
      )

      def forkArgs = Seq(
        "--add-modules",
        "jdk.incubator.foreign",
        "--enable-native-access",
        "ALL-UNNAMED"
      )

      def ivyDeps = Agg(ivy"org.scalameta::munit::${v.munit}")
   }

   object bench extends Benchmarks {
      def jmhVersion = "1.33"

      def forkArgs = Seq(
        "--add-modules",
        "jdk.incubator.foreign",
        "--enable-native-access",
        "ALL-UNNAMED"
      )

   }
}

object openblas extends ScalaModule with benchmark.BenchmarksModule {
   def scalaVersion = "3.1.1"
   def moduleDeps = Seq(slinc)

   object bench extends Benchmarks {
      def jmhVersion = "1.33"

      def forkArgs = Seq(
        "--add-modules=jdk.incubator.foreign",
        "--enable-native-access=ALL-UNNAMED"
      )

      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.jblas:jblas:1.2.5"
      )
   }
}

object polymorphics extends ScalaModule with publishable.PublishableModule {
   def publishVersion = "0.1.1"
   def mimaPreviousVersions = Seq("0.1.1")
   def scalaVersion = "2.13.7"
   def pomSettings = pomTemplate(
     "Shim to use polymorphic methods from scala 3 <DON'T DEPEND ON ME>"
   )
   def scalacPluginIvyDeps = Agg(ivy"org.scalameta:::semanticdb-scalac:4.4.32")
   def scalacOptions = Seq("-Yrangepos")
}
