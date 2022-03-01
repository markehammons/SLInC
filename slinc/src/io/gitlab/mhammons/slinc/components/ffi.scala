package io.gitlab.mhammons.slinc.components


import io.gitlab.mhammons.slincffi.{FFI, FFI17, FFI18}

val ffi: FFI = System.getProperty("java.version") match 
   case FFI17(ffi) => ffi
   case FFI18(ffi) => ffi