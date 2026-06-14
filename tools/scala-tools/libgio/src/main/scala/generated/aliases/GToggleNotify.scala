package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GToggleNotify:

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gobject/gobject.h
*/
opaque type GToggleNotify = CFuncPtr3[gpointer, Ptr[GObject], gboolean, Unit]
object GToggleNotify:
  given _tag: Tag[GToggleNotify] = Tag.materializeCFuncPtr3[gpointer, Ptr[GObject], gboolean, Unit]
  inline def fromPtr(ptr: Ptr[Byte] | CVoidPtr): GToggleNotify = CFuncPtr.fromPtr(ptr.asInstanceOf[Ptr[Byte]])
  inline def apply(inline o: CFuncPtr3[gpointer, Ptr[GObject], gboolean, Unit]): GToggleNotify = o
  extension (v: GToggleNotify)
    inline def value: CFuncPtr3[gpointer, Ptr[GObject], gboolean, Unit] = v
    inline def toPtr: CVoidPtr = CFuncPtr.toPtr(v)