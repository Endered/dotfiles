package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusobjectmanagerserver.h
*/
opaque type GDBusObjectManagerServer = CStruct2[GObject, Ptr[GDBusObjectManagerServerPrivate]]

object GDBusObjectManagerServer:
  given _tag: Tag[GDBusObjectManagerServer] = Tag.materializeCStruct2Tag[GObject, Ptr[GDBusObjectManagerServerPrivate]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GDBusObjectManagerServer)
      inline def parent_instance : GObject = struct._1
      inline def parent_instance_=(value: GObject): Unit = (!struct.at1 = value)
      inline def priv : Ptr[GDBusObjectManagerServerPrivate] = struct._2
      inline def priv_=(value: Ptr[GDBusObjectManagerServerPrivate]): Unit = (!struct.at2 = value)
    end extension
  
  // Allocates GDBusObjectManagerServer on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GDBusObjectManagerServer] = scala.scalanative.unsafe.alloc[GDBusObjectManagerServer](1)
  def apply(parent_instance : GObject, priv : Ptr[GDBusObjectManagerServerPrivate])(using Zone): Ptr[GDBusObjectManagerServer] =
    val ____ptr = apply()
    (!____ptr).parent_instance = parent_instance
    (!____ptr).priv = priv
    ____ptr