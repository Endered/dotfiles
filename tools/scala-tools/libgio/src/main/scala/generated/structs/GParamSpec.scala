package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gobject/gparam.h
*/
opaque type GParamSpec = CStruct10[GTypeInstance, Ptr[gchar], GParamFlags, GType, GType, Ptr[gchar], Ptr[gchar], Ptr[GData], guint, guint]

object GParamSpec:
  given _tag: Tag[GParamSpec] = Tag.materializeCStruct10Tag[GTypeInstance, Ptr[gchar], GParamFlags, GType, GType, Ptr[gchar], Ptr[gchar], Ptr[GData], guint, guint]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GParamSpec)
      inline def g_type_instance : GTypeInstance = struct._1
      inline def g_type_instance_=(value: GTypeInstance): Unit = (!struct.at1 = value)
      inline def name : Ptr[gchar] = struct._2
      inline def name_=(value: Ptr[gchar]): Unit = (!struct.at2 = value)
      inline def flags : GParamFlags = struct._3
      inline def flags_=(value: GParamFlags): Unit = (!struct.at3 = value)
      inline def value_type : GType = struct._4
      inline def value_type_=(value: GType): Unit = (!struct.at4 = value)
      inline def owner_type : GType = struct._5
      inline def owner_type_=(value: GType): Unit = (!struct.at5 = value)
      inline def _nick : Ptr[gchar] = struct._6
      inline def _nick_=(value: Ptr[gchar]): Unit = (!struct.at6 = value)
      inline def _blurb : Ptr[gchar] = struct._7
      inline def _blurb_=(value: Ptr[gchar]): Unit = (!struct.at7 = value)
      inline def qdata : Ptr[GData] = struct._8
      inline def qdata_=(value: Ptr[GData]): Unit = (!struct.at8 = value)
      inline def ref_count : guint = struct._9
      inline def ref_count_=(value: guint): Unit = (!struct.at9 = value)
      inline def param_id : guint = struct._10
      inline def param_id_=(value: guint): Unit = (!struct.at10 = value)
    end extension
  
  // Allocates GParamSpec on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GParamSpec] = scala.scalanative.unsafe.alloc[GParamSpec](1)
  def apply(g_type_instance : GTypeInstance, name : Ptr[gchar], flags : GParamFlags, value_type : GType, owner_type : GType, _nick : Ptr[gchar], _blurb : Ptr[gchar], qdata : Ptr[GData], ref_count : guint, param_id : guint)(using Zone): Ptr[GParamSpec] =
    val ____ptr = apply()
    (!____ptr).g_type_instance = g_type_instance
    (!____ptr).name = name
    (!____ptr).flags = flags
    (!____ptr).value_type = value_type
    (!____ptr).owner_type = owner_type
    (!____ptr)._nick = _nick
    (!____ptr)._blurb = _blurb
    (!____ptr).qdata = qdata
    (!____ptr).ref_count = ref_count
    (!____ptr).param_id = param_id
    ____ptr