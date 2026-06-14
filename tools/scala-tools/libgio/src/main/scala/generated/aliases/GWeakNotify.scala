package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GWeakNotify:

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gobject/gobject.h
*/
opaque type GWeakNotify = CFuncPtr2[gpointer, Ptr[GObject], Unit]
object GWeakNotify:
  given _tag: Tag[GWeakNotify] = Tag.materializeCFuncPtr2[gpointer, Ptr[GObject], Unit]
  inline def fromPtr(ptr: Ptr[Byte] | CVoidPtr): GWeakNotify = CFuncPtr.fromPtr(ptr.asInstanceOf[Ptr[Byte]])
  inline def apply(inline o: CFuncPtr2[gpointer, Ptr[GObject], Unit]): GWeakNotify = o
  extension (v: GWeakNotify)
    inline def value: CFuncPtr2[gpointer, Ptr[GObject], Unit] = v
    inline def toPtr: CVoidPtr = CFuncPtr.toPtr(v)