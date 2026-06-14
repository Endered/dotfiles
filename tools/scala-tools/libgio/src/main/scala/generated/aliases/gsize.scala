package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
opaque type gsize = CUnsignedLongInt
object gsize:
  given _tag: Tag[gsize] = Tag.USize
  inline def apply(inline o: CUnsignedLongInt): gsize = o
  extension (v: gsize)
    inline def value: CUnsignedLongInt = v