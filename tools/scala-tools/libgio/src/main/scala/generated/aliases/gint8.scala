package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /nix/store/yxyncg0y2b39kjgi4c52w74c4w1za7pg-glib-2.82.5/lib/glib-2.0/include/glibconfig.h
*/
opaque type gint8 = CChar
object gint8:
  given _tag: Tag[gint8] = Tag.Byte
  inline def apply(inline o: CChar): gint8 = o
  extension (v: gint8)
    inline def value: CChar = v