package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusProxyTypeFunc: : A #GDBusObjectManagerClient. _path: The object path of the remote object.

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/giotypes.h
*/
opaque type GDBusProxyTypeFunc = CFuncPtr4[Ptr[GDBusObjectManagerClient], Ptr[gchar], Ptr[gchar], gpointer, GType]
object GDBusProxyTypeFunc:
  given _tag: Tag[GDBusProxyTypeFunc] = Tag.materializeCFuncPtr4[Ptr[GDBusObjectManagerClient], Ptr[gchar], Ptr[gchar], gpointer, GType]
  inline def fromPtr(ptr: Ptr[Byte] | CVoidPtr): GDBusProxyTypeFunc = CFuncPtr.fromPtr(ptr.asInstanceOf[Ptr[Byte]])
  inline def apply(inline o: CFuncPtr4[Ptr[GDBusObjectManagerClient], Ptr[gchar], Ptr[gchar], gpointer, GType]): GDBusProxyTypeFunc = o
  extension (v: GDBusProxyTypeFunc)
    inline def value: CFuncPtr4[Ptr[GDBusObjectManagerClient], Ptr[gchar], Ptr[gchar], gpointer, GType] = v
    inline def toPtr: CVoidPtr = CFuncPtr.toPtr(v)