package io.gitlab.mhammons.slinc.components

import java.util.concurrent.atomic.AtomicReference
import scala.reflect.ClassTag
import java.util.concurrent.atomic.AtomicMarkableReference

class GrowingArray[T <: AnyRef: ClassTag](originalSize: Int) {
   private val array = AtomicReference(
     Array.fill[AtomicMarkableReference[T]](originalSize)(
       AtomicMarkableReference[T](null.asInstanceOf[T], false)
     )
   )

   def resizeArray(desiredIndex: Int): Unit =
      val arr = array.get

      val success = if arr.length <= desiredIndex then
         var proposedLength = arr.length * 2
         while proposedLength <= desiredIndex do proposedLength *= 2
         println(arr.length)
         val nArr = Array.fill[AtomicMarkableReference[T]](proposedLength)(
           AtomicMarkableReference[T](null.asInstanceOf[T], false)
         )
         arr.copyToArray(nArr)
         array.compareAndSet(arr, nArr)
      else true

      if success then () else resizeArray(desiredIndex)

   inline def get(index: Int, inline t: T): T =
      val arr = array.get
      if arr.length <= index then
         println("unhappiest path. allocating new array")
         resizeArray(index)
         println("array resized")
         val nArr = array.get
         println(nArr.length)
         val value = t
         nArr(index).compareAndSet(null.asInstanceOf[T], value, false, true)
         value
      else if !arr(index).isMarked then
         val value = t
         println(s"unhappy path, caching value $value")
         arr(index).compareAndSet(null.asInstanceOf[T], value, false, true)
         value
      else arr(index).getReference
}
