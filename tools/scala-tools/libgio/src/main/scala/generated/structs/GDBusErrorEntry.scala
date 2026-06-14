package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusErrorEntry: _code: An error code. _error_name: The D-Bus error name to associate with _code.

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbuserror.h
*/
opaque type GDBusErrorEntry = CStruct2[gint, Ptr[gchar]]

object GDBusErrorEntry:
  given _tag: Tag[GDBusErrorEntry] = Tag.materializeCStruct2Tag[gint, Ptr[gchar]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GDBusErrorEntry)
      inline def error_code : gint = struct._1
      inline def error_code_=(value: gint): Unit = (!struct.at1 = value)
      inline def dbus_error_name : Ptr[gchar] = struct._2
      inline def dbus_error_name_=(value: Ptr[gchar]): Unit = (!struct.at2 = value)
    end extension
  
  // Allocates GDBusErrorEntry on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GDBusErrorEntry] = scala.scalanative.unsafe.alloc[GDBusErrorEntry](1)
  def apply(error_code : gint, dbus_error_name : Ptr[gchar])(using Zone): Ptr[GDBusErrorEntry] =
    val ____ptr = apply()
    (!____ptr).error_code = error_code
    (!____ptr).dbus_error_name = dbus_error_name
    ____ptr