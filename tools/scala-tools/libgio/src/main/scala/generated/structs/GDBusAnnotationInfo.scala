package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusAnnotationInfo: _count: The reference count or -1 if statically allocated. : The name of the annotation, e.g. "org.freedesktop.DBus.Deprecated". : The value of the annotation. : (array zero-terminated=1): A pointer to a %NULL-terminated array of pointers to #GDBusAnnotationInfo structures or %NULL if there are no annotations.

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusintrospection.h
*/
opaque type GDBusAnnotationInfo = CStruct4[gint, Ptr[gchar], Ptr[gchar], Ptr[Byte]]

object GDBusAnnotationInfo:
  given _tag: Tag[GDBusAnnotationInfo] = Tag.materializeCStruct4Tag[gint, Ptr[gchar], Ptr[gchar], Ptr[Byte]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GDBusAnnotationInfo)
      inline def ref_count : gint = struct._1
      inline def ref_count_=(value: gint): Unit = (!struct.at1 = value)
      inline def key : Ptr[gchar] = struct._2
      inline def key_=(value: Ptr[gchar]): Unit = (!struct.at2 = value)
      inline def value : Ptr[gchar] = struct._3
      inline def value_=(value: Ptr[gchar]): Unit = (!struct.at3 = value)
      inline def annotations : Ptr[Ptr[GDBusAnnotationInfo]] = struct._4.asInstanceOf[Ptr[Ptr[GDBusAnnotationInfo]]]
      inline def annotations_=(value: Ptr[Ptr[GDBusAnnotationInfo]]): Unit = (!struct.at4 = value.asInstanceOf[Ptr[Byte]])
    end extension
  
  // Allocates GDBusAnnotationInfo on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GDBusAnnotationInfo] = scala.scalanative.unsafe.alloc[GDBusAnnotationInfo](1)
  def apply(ref_count : gint, key : Ptr[gchar], value : Ptr[gchar], annotations : Ptr[Ptr[GDBusAnnotationInfo]])(using Zone): Ptr[GDBusAnnotationInfo] =
    val ____ptr = apply()
    (!____ptr).ref_count = ref_count
    (!____ptr).key = key
    (!____ptr).value = value
    (!____ptr).annotations = annotations
    ____ptr