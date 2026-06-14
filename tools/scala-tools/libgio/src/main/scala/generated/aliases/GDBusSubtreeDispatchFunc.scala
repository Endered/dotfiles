package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusSubtreeDispatchFunc: : A #GDBusConnection. : The unique bus name of the remote caller. _path: The object path that was registered with g_dbus_connection_register_subtree(). : A node that is a child of _path (relative to _path) or %NULL for the root of the subtree. _user_data: (nullable) (not optional): Return location for user data to pass to functions in the returned #GDBusInterfaceVTable. _data: The _data #gpointer passed to g_dbus_connection_register_subtree().

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusconnection.h
*/
opaque type GDBusSubtreeDispatchFunc = CFuncPtr7[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gpointer], gpointer, Ptr[GDBusInterfaceVTable]]
object GDBusSubtreeDispatchFunc:
  given _tag: Tag[GDBusSubtreeDispatchFunc] = Tag.materializeCFuncPtr7[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gpointer], gpointer, Ptr[GDBusInterfaceVTable]]
  inline def fromPtr(ptr: Ptr[Byte] | CVoidPtr): GDBusSubtreeDispatchFunc = CFuncPtr.fromPtr(ptr.asInstanceOf[Ptr[Byte]])
  inline def apply(inline o: CFuncPtr7[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gpointer], gpointer, Ptr[GDBusInterfaceVTable]]): GDBusSubtreeDispatchFunc = o
  extension (v: GDBusSubtreeDispatchFunc)
    inline def value: CFuncPtr7[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gpointer], gpointer, Ptr[GDBusInterfaceVTable]] = v
    inline def toPtr: CVoidPtr = CFuncPtr.toPtr(v)