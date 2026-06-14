package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /nix/store/yxyncg0y2b39kjgi4c52w74c4w1za7pg-glib-2.82.5/lib/glib-2.0/include/glibconfig.h
*/
opaque type gint64 = CLongInt
object gint64:
  given _tag: Tag[gint64] = Tag.Size
  inline def apply(inline o: CLongInt): gint64 = o
  extension (v: gint64)
    inline def value: CLongInt = v