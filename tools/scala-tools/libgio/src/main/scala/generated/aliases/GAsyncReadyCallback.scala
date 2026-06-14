package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GAsyncReadyCallback: _object: (nullable): the object the asynchronous operation was started with. : a #GAsyncResult.

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/giotypes.h
*/
opaque type GAsyncReadyCallback = CFuncPtr3[Ptr[GObject], Ptr[GAsyncResult], gpointer, Unit]
object GAsyncReadyCallback:
  given _tag: Tag[GAsyncReadyCallback] = Tag.materializeCFuncPtr3[Ptr[GObject], Ptr[GAsyncResult], gpointer, Unit]
  inline def fromPtr(ptr: Ptr[Byte] | CVoidPtr): GAsyncReadyCallback = CFuncPtr.fromPtr(ptr.asInstanceOf[Ptr[Byte]])
  inline def apply(inline o: CFuncPtr3[Ptr[GObject], Ptr[GAsyncResult], gpointer, Unit]): GAsyncReadyCallback = o
  extension (v: GAsyncReadyCallback)
    inline def value: CFuncPtr3[Ptr[GObject], Ptr[GAsyncResult], gpointer, Unit] = v
    inline def toPtr: CVoidPtr = CFuncPtr.toPtr(v)