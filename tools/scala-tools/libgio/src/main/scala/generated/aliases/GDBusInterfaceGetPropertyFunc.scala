package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusInterfaceGetPropertyFunc: : A #GDBusConnection. : The unique bus name of the remote caller. _path: The object path that the method was invoked on. : Return location for error. _data: The _data #gpointer passed to g_dbus_connection_register_object().

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusconnection.h
*/
opaque type GDBusInterfaceGetPropertyFunc = CFuncPtr7[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[Ptr[GError]], gpointer, Ptr[GVariant]]
object GDBusInterfaceGetPropertyFunc:
  given _tag: Tag[GDBusInterfaceGetPropertyFunc] = Tag.materializeCFuncPtr7[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[Ptr[GError]], gpointer, Ptr[GVariant]]
  inline def fromPtr(ptr: Ptr[Byte] | CVoidPtr): GDBusInterfaceGetPropertyFunc = CFuncPtr.fromPtr(ptr.asInstanceOf[Ptr[Byte]])
  inline def apply(inline o: CFuncPtr7[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[Ptr[GError]], gpointer, Ptr[GVariant]]): GDBusInterfaceGetPropertyFunc = o
  extension (v: GDBusInterfaceGetPropertyFunc)
    inline def value: CFuncPtr7[Ptr[GDBusConnection], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[gchar], Ptr[Ptr[GError]], gpointer, Ptr[GVariant]] = v
    inline def toPtr: CVoidPtr = CFuncPtr.toPtr(v)