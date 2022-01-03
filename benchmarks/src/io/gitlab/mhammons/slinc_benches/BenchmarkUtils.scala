package io.gitlab.mhammons.slinc.benches

def repeat[A](fn: => A, times: Int) =
   var count = times
   while count > 0 do
      fn
      count -= 1

inline def repeatInl[A](inline fn: A, times: Int) =
   var count = times
   while count > 0 do
      fn
      count -= 1
