package io.gitlab.mhammons.slinc.components

import ffi.{Linker => CLinker}

object Linker:
  val linker = CLinker.systemCLinker()

