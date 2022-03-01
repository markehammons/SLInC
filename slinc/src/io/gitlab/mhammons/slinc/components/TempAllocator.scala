package io.gitlab.mhammons.slinc.components

import ffi.{
   Segment,
   Allocator,
   Scope,
   Address
}

private val allocator = ThreadLocal.withInitial(() => TempAllocator())

sealed class TempAllocator:
   private val powersOf2 = LazyList.iterate(1L)(_ << 1).iterator
   private var offset = 0L
   private var rs: Scope = Scope.newConfinedScope()
   private var allocator: Allocator = Allocator.arenaAllocator(rs)
   private var currentSegment: Segment =
      val next = powersOf2.next

      allocator.allocateSegment(next)

   private var needsCleaning = false

   def allocate(bytes: Long): Segment =
      if bytes + offset > currentSegment.byteSize then
         offset = 0L
         var nextSize = powersOf2.next
         while nextSize < bytes do nextSize = powersOf2.next
         currentSegment = allocator.allocateSegment(nextSize)

         allocate(bytes)
      else
         val oldOffset = offset
         offset += bytes
         currentSegment.asSlice(oldOffset, bytes)

   def reset() =
      offset = 0L
      if needsCleaning then 
         val oldRs = rs 
         rs = Scope.newConfinedScope()
         allocator = Allocator.arenaAllocator(rs)
         val nSegment = allocator.allocateSegment(currentSegment.byteSize)
         currentSegment.copyFrom(nSegment)
         oldRs.close()
         needsCleaning = false

private def reset() =
   allocator.get.reset()

private val localAllocator: Allocator = Allocator.genAllocator(bytesNeeded =>
   allocator.get.allocate(bytesNeeded)
)

private val powersOf2 = LazyList.iterate(1L)(_ << 1)
