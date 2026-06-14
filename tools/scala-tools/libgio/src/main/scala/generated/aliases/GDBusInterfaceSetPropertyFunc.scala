package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusInterfaceSetPropertyFunc: : A #GDBusConnection. : The unique bus name of the remote caller. _path: The object path that the method was invoked on. : The value to set the property to. : Return location for error. _data: The _data #gpointer passed to g_dbus_connection_register_object().

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusconnection.h
*/
opaque type GDBusInterfaceSetPropertyFunc = CFuncPtr8[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[GVariant], Ptr[Ptr[GError]], gpointer, gboolean]
object GDBusInterfaceSetPropertyFunc:
  given _tag: Tag[GDBusInterfaceSetPropertyFunc] = Tag.materializeCFuncPtr8[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[GVariant], Ptr[Ptr[GError]], gpointer, gboolean]
  inline def fromPtr(ptr: Ptr[Byte] | CVoidPtr): GDBusInterfaceSetPropertyFunc = CFuncPtr.fromPtr(ptr.asInstanceOf[Ptr[Byte]])
  inline def apply(inline o: CFuncPtr8[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[GVariant], Ptr[Ptr[GError]], gpointer, gboolean]): GDBusInterfaceSetPropertyFunc = o
  extension (v: GDBusInterfaceSetPropertyFunc)
    inline def value: CFuncPtr8[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[GVariant], Ptr[Ptr[GError]], gpointer, gboolean] = v
    inline def toPtr: CVoidPtr = CFuncPtr.toPtr(v)