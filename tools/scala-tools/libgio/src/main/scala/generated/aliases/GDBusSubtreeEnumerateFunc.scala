package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusSubtreeEnumerateFunc: : A #GDBusConnection. : The unique bus name of the remote caller. _path: The object path that was registered with g_dbus_connection_register_subtree(). _data: The _data #gpointer passed to g_dbus_connection_register_subtree().

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusconnection.h
*/
opaque type GDBusSubtreeEnumerateFunc = CFuncPtr4[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], gpointer, Ptr[Ptr[gchar]]]
object GDBusSubtreeEnumerateFunc:
  given _tag: Tag[GDBusSubtreeEnumerateFunc] = Tag.materializeCFuncPtr4[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], gpointer, Ptr[Ptr[gchar]]]
  inline def fromPtr(ptr: Ptr[Byte] | CVoidPtr): GDBusSubtreeEnumerateFunc = CFuncPtr.fromPtr(ptr.asInstanceOf[Ptr[Byte]])
  inline def apply(inline o: CFuncPtr4[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], gpointer, Ptr[Ptr[gchar]]]): GDBusSubtreeEnumerateFunc = o
  extension (v: GDBusSubtreeEnumerateFunc)
    inline def value: CFuncPtr4[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], gpointer, Ptr[Ptr[gchar]]] = v
    inline def toPtr: CVoidPtr = CFuncPtr.toPtr(v)