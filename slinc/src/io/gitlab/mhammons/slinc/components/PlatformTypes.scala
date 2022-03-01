package io.gitlab.mhammons.slinc.components

trait Platform
    extends IntptrTProto,
      UintptrTProto,
      /*CaddrTProto,*/ DevTProto,
      BlkcntTProto,
      BlksizeTProto,
      GidTProto,
      InAddrTProto,
      InPortTProto,
      InoTProto,
      Ino64TProto,
      KeyTProto,
      ModeTProto,
      NlinkTProto,
      IdTProto,
      PidTProto,
      OffTProto,
      SwblkTProto,
      UidTProto,
      ClockTProto,
      SizeTProto,
      SsizeTProto,
      TimeTProto,
      FsblkcntTProto,
      FsfilcntTProto,
      SaFamilyTProto,
      SocklenTProto,
      RlimTProto,
      CcTProto,
      SpeedTProto,
      TcflagTProto:
   type int8_t = Byte
   type u_int8_t = Byte
   type int16_t = Short
   type u_int16_t = Short
   type int32_t = Int
   type u_int32_t = Int
   type int64_t = Long
   type u_int64_t = Long
   type Int8T = int8_t
   type UInt8T = u_int8_t
   type Int16T = int16_t
   type UInt16T = u_int16_t
   type Int32T = int32_t
   type UInt32T = u_int32_t
   type Int64T = int64_t
   type UInt64T = u_int64_t
   type UByte = UInt8T
   type UShort = UInt16T
   type UInt = UInt32T
   type ULong = UInt64T

case object PlatformX64Linux
    extends Platform,
      IntptrTImpl[Platform#Int64T],
      UintptrTImpl[Platform#UInt64T],
      DevTImpl[Platform#UInt64T],
      BlkcntTImpl[Platform#Int64T],
      BlksizeTImpl[Platform#Int64T],
      GidTImpl[Platform#UInt32T],
      InAddrTImpl[Platform#UInt32T],
      InPortTImpl[Platform#UInt16T],
      InoTImpl[Platform#UInt64T],
      Ino64TImpl[Platform#UInt64T],
      KeyTImpl[Platform#Int32T],
      ModeTImpl[Platform#UInt32T],
      NlinkTImpl[Platform#UInt64T],
      IdTImpl[Platform#UInt32T],
      PidTImpl[Platform#Int32T],
      OffTImpl[Platform#Int64T],
      SwblkTImpl[Platform#Int64T],
      UidTImpl[Platform#UInt32T],
      ClockTImpl[Platform#Int64T],
      SizeTImpl[Platform#UInt64T],
      SsizeTImpl[Platform#Int64T],
      TimeTImpl[Platform#Int64T],
      FsblkcntTImpl[Platform#UInt64T],
      FsfilcntTImpl[Platform#UInt64T],
      SaFamilyTImpl[Platform#UInt16T],
      SocklenTImpl[Platform#UInt32T],
      RlimTImpl[Platform#UInt64T],
      CcTImpl[Platform#UInt8T],
      SpeedTImpl[Platform#UInt32T],
      TcflagTImpl[Platform#UInt32T]

val platform: Platform = (arch, os) match
   case (Arch.X86_64, OS.Linux) => PlatformX64Linux
   case _ => throw new Error("Platform isn't supported...")
