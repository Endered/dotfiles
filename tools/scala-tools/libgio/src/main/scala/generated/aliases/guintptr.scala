package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /nix/store/yxyncg0y2b39kjgi4c52w74c4w1za7pg-glib-2.82.5/lib/glib-2.0/include/glibconfig.h
*/
opaque type guintptr = CUnsignedLongInt
object guintptr:
  given _tag: Tag[guintptr] = Tag.USize
  inline def apply(inline o: CUnsignedLongInt): guintptr = o
  extension (v: guintptr)
    inline def value: CUnsignedLongInt = v