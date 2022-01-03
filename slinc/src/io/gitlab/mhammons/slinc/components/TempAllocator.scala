package io.gitlab.mhammons.slinc.components

import jdk.incubator.foreign.{
   MemorySegment,
   SegmentAllocator,
   CLinker,
   ResourceScope,
   MemoryAddress
}

private val allocator = ThreadLocal.withInitial(() => TempAllocator())

sealed class TempAllocator:
   private val powersOf2 = LazyList.iterate(1L)(_ << 1).iterator
   private var currentSegment =
      val next = powersOf2.next
      CLinker.allocateMemory(next).asSegment(next, ResourceScope.globalScope)
   private var offset = 0L
   private var rs: () => Unit = null

   private def addToRs(memoryAddress: MemoryAddress) =
      if rs != null then
         val curRs = rs
         rs = () => { curRs(); CLinker.freeMemory(memoryAddress) }
      else rs = () => CLinker.freeMemory(memoryAddress)

   def allocate(bytes: Long): MemorySegment =
      if bytes + offset > currentSegment.byteSize then
         println(
           s"reallocating to fit cause total bytes needed is at ${offset + bytes} and current segment has size of ${currentSegment.byteSize}"
         )
         addToRs(currentSegment.address)
         offset = 0L
         var nextSize = powersOf2.next
         while nextSize < bytes do nextSize = powersOf2.next
         println(nextSize)
         currentSegment =
            MemorySegment.allocateNative(nextSize, ResourceScope.globalScope)

         allocate(bytes)
      else
         val oldOffset = offset
         offset += bytes
         currentSegment.asSlice(oldOffset, bytes)

   def reset() =
      offset = 0L
      if rs != null then
         rs()
         rs = null

private def reset() =
   allocator.get.reset()

private val localAllocator: SegmentAllocator = (bytesNeeded, alignment) =>
   allocator.get.allocate(bytesNeeded)

private val powersOf2 = LazyList.iterate(1L)(_ << 1)
private val nextSizeLocal = ThreadLocal.withInitial(() => powersOf2.iterator)
