package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusSignalInfo: _count: The reference count or -1 if statically allocated.

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusintrospection.h
*/
opaque type GDBusSignalInfo = CStruct4[gint, Ptr[gchar], Ptr[Byte], Ptr[Byte]]

object GDBusSignalInfo:
  given _tag: Tag[GDBusSignalInfo] = Tag.materializeCStruct4Tag[gint, Ptr[gchar], Ptr[Byte], Ptr[Byte]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GDBusSignalInfo)
      inline def ref_count : gint = struct._1
      inline def ref_count_=(value: gint): Unit = (!struct.at1 = value)
      inline def name : Ptr[gchar] = struct._2
      inline def name_=(value: Ptr[gchar]): Unit = (!struct.at2 = value)
      inline def args : Ptr[Ptr[GDBusArgInfo]] = struct._3.asInstanceOf[Ptr[Ptr[GDBusArgInfo]]]
      inline def args_=(value: Ptr[Ptr[GDBusArgInfo]]): Unit = (!struct.at3 = value.asInstanceOf[Ptr[Byte]])
      inline def annotations : Ptr[Ptr[GDBusAnnotationInfo]] = struct._4.asInstanceOf[Ptr[Ptr[GDBusAnnotationInfo]]]
      inline def annotations_=(value: Ptr[Ptr[GDBusAnnotationInfo]]): Unit = (!struct.at4 = value.asInstanceOf[Ptr[Byte]])
    end extension
  
  // Allocates GDBusSignalInfo on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GDBusSignalInfo] = scala.scalanative.unsafe.alloc[GDBusSignalInfo](1)
  def apply(ref_count : gint, name : Ptr[gchar], args : Ptr[Ptr[GDBusArgInfo]], annotations : Ptr[Ptr[GDBusAnnotationInfo]])(using Zone): Ptr[GDBusSignalInfo] =
    val ____ptr = apply()
    (!____ptr).ref_count = ref_count
    (!____ptr).name = name
    (!____ptr).args = args
    (!____ptr).annotations = annotations
    ____ptr