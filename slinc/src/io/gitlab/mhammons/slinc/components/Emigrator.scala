package io.gitlab.mhammons.slinc.components

import jdk.incubator.foreign.{CLinker, SegmentAllocator}

type Emigaratee[A, B] = Emigrator[A] ?=> B

def emigrator[A]: Emigaratee[A, Emigrator[A]] = summon[Emigrator[A]]

trait Emigrator[A]:
   def apply(a: A): Allocatee[Any]

object Emigrator:
   given Emigrator[Int] with
      def apply(a: Int) = a

   given Emigrator[Long] with
      def apply(a: Long) = a
   given Emigrator[String] with
      def apply(a: String) = CLinker.toCString(a, segAlloc).address

   given Emigrator[Double] with
      def apply(a: Double) = a

   given Emigrator[Float] with
      def apply(a: Float) = a
