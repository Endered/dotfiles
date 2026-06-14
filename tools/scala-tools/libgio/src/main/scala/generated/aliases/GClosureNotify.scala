package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GClosureNotify:

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gobject/gclosure.h
*/
opaque type GClosureNotify = CFuncPtr2[gpointer, Ptr[GClosure], Unit]
object GClosureNotify:
  given _tag: Tag[GClosureNotify] = Tag.materializeCFuncPtr2[gpointer, Ptr[GClosure], Unit]
  inline def fromPtr(ptr: Ptr[Byte] | CVoidPtr): GClosureNotify = CFuncPtr.fromPtr(ptr.asInstanceOf[Ptr[Byte]])
  inline def apply(inline o: CFuncPtr2[gpointer, Ptr[GClosure], Unit]): GClosureNotify = o
  extension (v: GClosureNotify)
    inline def value: CFuncPtr2[gpointer, Ptr[GClosure], Unit] = v
    inline def toPtr: CVoidPtr = CFuncPtr.toPtr(v)