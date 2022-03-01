package io.gitlab.mhammons.slinc.components

import scala.annotation.targetName

type Emigratee[A, B] = Emigrator[A] ?=> B

def emigrator[A]: Emigratee[A, Emigrator[A]] = summon[Emigrator[A]]
def emigrate[A](a: A): Allocatee[Emigratee[A, Any]] = emigrator[A](a)

trait Emigrator[A]:
   def apply(a: A): Allocatee[Any]
   @targetName("contramapEm")
   def contramap[B](fn: B => A): Emigrator[B] =
      val orig = this
      new Emigrator[B]:
         def apply(b: B) = orig(fn(b))

object Emigrator:
   given Emigrator[Int] with
      def apply(a: Int) = a

   given Emigrator[Long] with
      def apply(a: Long) = a

   given Emigrator[Double] with
      def apply(a: Double) = a

   given Emigrator[Float] with
      def apply(a: Float) = a

   given Emigrator[Byte] with
      def apply(a: Byte) = a

   given Emigrator[Short] with
      def apply(a: Short) = a

   given Emigrator[Boolean] =
      emigrator[Byte].contramap[Boolean](if _ then 0 else 1)

// given Emigrator[Char] =
