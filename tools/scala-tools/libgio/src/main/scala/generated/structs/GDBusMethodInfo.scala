package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusMethodInfo: _count: The reference count or -1 if statically allocated. _args: (array zero-terminated=1): A pointer to a %NULL-terminated array of pointers to #GDBusArgInfo structures or %NULL if there are no in arguments. _args: (array zero-terminated=1): A pointer to a %NULL-terminated array of pointers to #GDBusArgInfo structures or %NULL if there are no out arguments. : (array zero-terminated=1): A pointer to a %NULL-terminated array of pointers to #GDBusAnnotationInfo structures or %NULL if there are no annotations.

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusintrospection.h
*/
opaque type GDBusMethodInfo = CStruct5[gint, Ptr[gchar], Ptr[Byte], Ptr[Byte], Ptr[Byte]]

object GDBusMethodInfo:
  given _tag: Tag[GDBusMethodInfo] = Tag.materializeCStruct5Tag[gint, Ptr[gchar], Ptr[Byte], Ptr[Byte], Ptr[Byte]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GDBusMethodInfo)
      inline def ref_count : gint = struct._1
      inline def ref_count_=(value: gint): Unit = (!struct.at1 = value)
      inline def name : Ptr[gchar] = struct._2
      inline def name_=(value: Ptr[gchar]): Unit = (!struct.at2 = value)
      inline def in_args : Ptr[Ptr[GDBusArgInfo]] = struct._3.asInstanceOf[Ptr[Ptr[GDBusArgInfo]]]
      inline def in_args_=(value: Ptr[Ptr[GDBusArgInfo]]): Unit = (!struct.at3 = value.asInstanceOf[Ptr[Byte]])
      inline def out_args : Ptr[Ptr[GDBusArgInfo]] = struct._4.asInstanceOf[Ptr[Ptr[GDBusArgInfo]]]
      inline def out_args_=(value: Ptr[Ptr[GDBusArgInfo]]): Unit = (!struct.at4 = value.asInstanceOf[Ptr[Byte]])
      inline def annotations : Ptr[Ptr[GDBusAnnotationInfo]] = struct._5.asInstanceOf[Ptr[Ptr[GDBusAnnotationInfo]]]
      inline def annotations_=(value: Ptr[Ptr[GDBusAnnotationInfo]]): Unit = (!struct.at5 = value.asInstanceOf[Ptr[Byte]])
    end extension
  
  // Allocates GDBusMethodInfo on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GDBusMethodInfo] = scala.scalanative.unsafe.alloc[GDBusMethodInfo](1)
  def apply(ref_count : gint, name : Ptr[gchar], in_args : Ptr[Ptr[GDBusArgInfo]], out_args : Ptr[Ptr[GDBusArgInfo]], annotations : Ptr[Ptr[GDBusAnnotationInfo]])(using Zone): Ptr[GDBusMethodInfo] =
    val ____ptr = apply()
    (!____ptr).ref_count = ref_count
    (!____ptr).name = name
    (!____ptr).in_args = in_args
    (!____ptr).out_args = out_args
    (!____ptr).annotations = annotations
    ____ptr