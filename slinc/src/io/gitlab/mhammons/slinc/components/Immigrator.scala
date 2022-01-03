package io.gitlab.mhammons.slinc.components

import jdk.incubator.foreign.{CLinker, MemoryAddress}

type Immigratee[A, B] = Immigrator[A] ?=> B

def immigrator[A]: Immigratee[A, Immigrator[A]] = summon[Immigrator[A]]

trait Immigrator[A]:
   def apply(a: Any): A

object Immigrator:
   given Immigrator[Unit] = _ => ()

   given Immigrator[Double] = _.asInstanceOf[Double]

   given Immigrator[Float] = _.asInstanceOf[Float]

   given Immigrator[String] = o =>
      CLinker.toJavaString(o.asInstanceOf[MemoryAddress])

   given Immigrator[Long] = _.asInstanceOf[Long]

   given Immigrator[Int] = _.asInstanceOf[Int]

   given Immigrator[Char] = _.asInstanceOf[Char]
