package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusobjectmanagerclient.h
*/
opaque type GDBusObjectManagerClient = CStruct2[GObject, Ptr[GDBusObjectManagerClientPrivate]]

object GDBusObjectManagerClient:
  given _tag: Tag[GDBusObjectManagerClient] = Tag.materializeCStruct2Tag[GObject, Ptr[GDBusObjectManagerClientPrivate]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GDBusObjectManagerClient)
      inline def parent_instance : GObject = struct._1
      inline def parent_instance_=(value: GObject): Unit = (!struct.at1 = value)
      inline def priv : Ptr[GDBusObjectManagerClientPrivate] = struct._2
      inline def priv_=(value: Ptr[GDBusObjectManagerClientPrivate]): Unit = (!struct.at2 = value)
    end extension
  
  // Allocates GDBusObjectManagerClient on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GDBusObjectManagerClient] = scala.scalanative.unsafe.alloc[GDBusObjectManagerClient](1)
  def apply(parent_instance : GObject, priv : Ptr[GDBusObjectManagerClientPrivate])(using Zone): Ptr[GDBusObjectManagerClient] =
    val ____ptr = apply()
    (!____ptr).parent_instance = parent_instance
    (!____ptr).priv = priv
    ____ptr