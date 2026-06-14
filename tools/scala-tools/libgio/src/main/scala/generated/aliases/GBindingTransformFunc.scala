package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GBindingTransformFunc: : a #GBinding _value: the #GValue containing the value to transform _value: the #GValue in which to store the transformed value _data: data passed to the transform function

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gobject/gbinding.h
*/
opaque type GBindingTransformFunc = CFuncPtr4[Ptr[GBinding], Ptr[GValue], Ptr[GValue], gpointer, gboolean]
object GBindingTransformFunc:
  given _tag: Tag[GBindingTransformFunc] = Tag.materializeCFuncPtr4[Ptr[GBinding], Ptr[GValue], Ptr[GValue], gpointer, gboolean]
  inline def fromPtr(ptr: Ptr[Byte] | CVoidPtr): GBindingTransformFunc = CFuncPtr.fromPtr(ptr.asInstanceOf[Ptr[Byte]])
  inline def apply(inline o: CFuncPtr4[Ptr[GBinding], Ptr[GValue], Ptr[GValue], gpointer, gboolean]): GBindingTransformFunc = o
  extension (v: GBindingTransformFunc)
    inline def value: CFuncPtr4[Ptr[GBinding], Ptr[GValue], Ptr[GValue], gpointer, gboolean] = v
    inline def toPtr: CVoidPtr = CFuncPtr.toPtr(v)