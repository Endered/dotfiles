package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusInterfaceVTable: _property: Function for getting a property.

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusconnection.h
*/
opaque type GDBusInterfaceVTable = CStruct4[GDBusInterfaceMethodCallFunc, GDBusInterfaceGetPropertyFunc, GDBusInterfaceSetPropertyFunc, CArray[gpointer, Nat._8]]

object GDBusInterfaceVTable:
  given _tag: Tag[GDBusInterfaceVTable] = Tag.materializeCStruct4Tag[GDBusInterfaceMethodCallFunc, GDBusInterfaceGetPropertyFunc, GDBusInterfaceSetPropertyFunc, CArray[gpointer, Nat._8]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GDBusInterfaceVTable)
      inline def method_call : GDBusInterfaceMethodCallFunc = struct._1
      inline def method_call_=(value: GDBusInterfaceMethodCallFunc): Unit = (!struct.at1 = value)
      inline def get_property : GDBusInterfaceGetPropertyFunc = struct._2
      inline def get_property_=(value: GDBusInterfaceGetPropertyFunc): Unit = (!struct.at2 = value)
      inline def set_property : GDBusInterfaceSetPropertyFunc = struct._3
      inline def set_property_=(value: GDBusInterfaceSetPropertyFunc): Unit = (!struct.at3 = value)
      inline def padding : CArray[gpointer, Nat._8] = struct._4
      inline def padding_=(value: CArray[gpointer, Nat._8]): Unit = (!struct.at4 = value)
    end extension
  
  // Allocates GDBusInterfaceVTable on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GDBusInterfaceVTable] = scala.scalanative.unsafe.alloc[GDBusInterfaceVTable](1)
  def apply(method_call : GDBusInterfaceMethodCallFunc, get_property : GDBusInterfaceGetPropertyFunc, set_property : GDBusInterfaceSetPropertyFunc, padding : CArray[gpointer, Nat._8])(using Zone): Ptr[GDBusInterfaceVTable] =
    val ____ptr = apply()
    (!____ptr).method_call = method_call
    (!____ptr).get_property = get_property
    (!____ptr).set_property = set_property
    (!____ptr).padding = padding
    ____ptr