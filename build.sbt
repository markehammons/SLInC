// lazy val root = project(file("."))

lazy val slinc = project
   .settings(
     Compile / scalaSource := baseDirectory.value / "src",
     Test / scalaSource := baseDirectory.value / "test",
     libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M1" % Test,
     scalaVersion := "3.1.0",
     Compile / sourceGenerators += Def.task {
        (for {
           (cTypeName, typeName) <- typeProtos
        } yield {
           val file = (Compile / sourceManaged).value / s"$typeName.scala"
           IO.write(file, makeSource(cTypeName, typeName))
           file
        }).toSeq
     }.taskValue
   )
   .dependsOn(polymorphics)

lazy val polymorphics = project.settings(
  scalaVersion := "2.13.7",
  Compile / scalaSource := baseDirectory.value / "src"
)

def makeSource(cTypeName: String, typeName: String) =
   s"""|package io.gitlab.mhammons.slinc.components
          |
          |trait ${typeName}Proto: 
          | type $typeName
          | type $cTypeName = $typeName
          | val ${typeName}Integral: Integral[$typeName]
          | implicit class ${typeName}Ops(a: $typeName) extends ${typeName}Integral.IntegralOps(a)
          | implicit class ${typeName}Ord(a: $typeName) extends ${typeName}Integral.OrderingOps(a)
          | given ${typeName}NativeInfo: NativeInfo[$typeName]
          | given ${typeName}Immigrator: Immigrator[$typeName]
          | given ${typeName}Emigrator: Emigrator[$typeName]
          | given ${typeName}Deserializer: Deserializer[$typeName]
          | given ${typeName}Serializer: Serializer[$typeName]
          | given ${typeName}Exporter: Exporter[$typeName]
          |
          |trait ${typeName}Impl[U](using 
          | val ${typeName}Integral: Integral[U], 
          | val ${typeName}NativeInfo: NativeInfo[U],
          | val ${typeName}Immigrator: Immigrator[U],
          | val ${typeName}Emigrator: Emigrator[U],
          | val ${typeName}Deserializer: Deserializer[U],
          | val ${typeName}Serializer: Serializer[U],
          | val ${typeName}Exporter: Exporter[U]
          |) extends ${typeName}Proto:
          | type $typeName = U""".stripMargin

val typeProtos = Set(
  "dev_t" -> "DevT",
  "size_t" -> "SizeT",
  "time_t" -> "TimeT",
  "clock_t" -> "ClockT",
  "intptr_t" -> "IntptrT",
  "uintptr_t" -> "UintptrT",
  "caddr_t" -> "CaddrT",
  "blkcnt_t" -> "BlkcntT",
  "blksize_t" -> "BlksizeT",
  "gid_t" -> "GidT",
  "in_addr_t" -> "InAddrT",
  "in_port_t" -> "InPortT",
  "ino_t" -> "InoT",
  "ino64_t" -> "Ino64T",
  "key_t" -> "KeyT",
  "nlink_t" -> "NlinkT",
  "id_t" -> "IdT",
  "pid_t" -> "PidT",
  "off_t" -> "OffT",
  "swblk_t" -> "SwblkT",
  "fsblkcnt_t" -> "FsblkcntT",
  "fsfilcnt_t" -> "FsfilcntT",
  "sa_family_t" -> "SaFamilyT",
  "socklen_t" -> "SocklenT",
  "rlim_t" -> "RlimT",
  "cc_t" -> "CcT",
  "speed_t" -> "SpeedT",
  "tcflag_t" -> "TcflagT",
  "mode_t" -> "ModeT",
  "ssize_t" -> "SsizeT",
  "uid_t" -> "UidT"
)
