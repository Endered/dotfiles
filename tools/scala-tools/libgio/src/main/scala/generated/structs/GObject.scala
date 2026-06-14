package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gobject/gobject.h
*/
opaque type GObject = CStruct3[GTypeInstance, guint, Ptr[GData]]

object GObject:
  given _tag: Tag[GObject] = Tag.materializeCStruct3Tag[GTypeInstance, guint, Ptr[GData]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GObject)
      inline def g_type_instance : GTypeInstance = struct._1
      inline def g_type_instance_=(value: GTypeInstance): Unit = (!struct.at1 = value)
      inline def ref_count : guint = struct._2
      inline def ref_count_=(value: guint): Unit = (!struct.at2 = value)
      inline def qdata : Ptr[GData] = struct._3
      inline def qdata_=(value: Ptr[GData]): Unit = (!struct.at3 = value)
    end extension
  
  // Allocates GObject on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GObject] = scala.scalanative.unsafe.alloc[GObject](1)
  def apply(g_type_instance : GTypeInstance, ref_count : guint, qdata : Ptr[GData])(using Zone): Ptr[GObject] =
    val ____ptr = apply()
    (!____ptr).g_type_instance = g_type_instance
    (!____ptr).ref_count = ref_count
    (!____ptr).qdata = qdata
    ____ptr