package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusInterfaceInfo: _count: The reference count or -1 if statically allocated. : (array zero-terminated=1): A pointer to a %NULL-terminated array of pointers to #GDBusSignalInfo structures or %NULL if there are no signals. : (array zero-terminated=1): A pointer to a %NULL-terminated array of pointers to #GDBusPropertyInfo structures or %NULL if there are no properties. : (array zero-terminated=1): A pointer to a %NULL-terminated array of pointers to #GDBusAnnotationInfo structures or %NULL if there are no annotations.

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusintrospection.h
*/
opaque type GDBusInterfaceInfo = CStruct6[gint, Ptr[gchar], Ptr[Byte], Ptr[Byte], Ptr[Byte], Ptr[Byte]]

object GDBusInterfaceInfo:
  given _tag: Tag[GDBusInterfaceInfo] = Tag.materializeCStruct6Tag[gint, Ptr[gchar], Ptr[Byte], Ptr[Byte], Ptr[Byte], Ptr[Byte]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GDBusInterfaceInfo)
      inline def ref_count : gint = struct._1
      inline def ref_count_=(value: gint): Unit = (!struct.at1 = value)
      inline def name : Ptr[gchar] = struct._2
      inline def name_=(value: Ptr[gchar]): Unit = (!struct.at2 = value)
      inline def methods : Ptr[Ptr[GDBusMethodInfo]] = struct._3.asInstanceOf[Ptr[Ptr[GDBusMethodInfo]]]
      inline def methods_=(value: Ptr[Ptr[GDBusMethodInfo]]): Unit = (!struct.at3 = value.asInstanceOf[Ptr[Byte]])
      inline def signals : Ptr[Ptr[GDBusSignalInfo]] = struct._4.asInstanceOf[Ptr[Ptr[GDBusSignalInfo]]]
      inline def signals_=(value: Ptr[Ptr[GDBusSignalInfo]]): Unit = (!struct.at4 = value.asInstanceOf[Ptr[Byte]])
      inline def properties : Ptr[Ptr[GDBusPropertyInfo]] = struct._5.asInstanceOf[Ptr[Ptr[GDBusPropertyInfo]]]
      inline def properties_=(value: Ptr[Ptr[GDBusPropertyInfo]]): Unit = (!struct.at5 = value.asInstanceOf[Ptr[Byte]])
      inline def annotations : Ptr[Ptr[GDBusAnnotationInfo]] = struct._6.asInstanceOf[Ptr[Ptr[GDBusAnnotationInfo]]]
      inline def annotations_=(value: Ptr[Ptr[GDBusAnnotationInfo]]): Unit = (!struct.at6 = value.asInstanceOf[Ptr[Byte]])
    end extension
  
  // Allocates GDBusInterfaceInfo on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GDBusInterfaceInfo] = scala.scalanative.unsafe.alloc[GDBusInterfaceInfo](1)
  def apply(ref_count : gint, name : Ptr[gchar], methods : Ptr[Ptr[GDBusMethodInfo]], signals : Ptr[Ptr[GDBusSignalInfo]], properties : Ptr[Ptr[GDBusPropertyInfo]], annotations : Ptr[Ptr[GDBusAnnotationInfo]])(using Zone): Ptr[GDBusInterfaceInfo] =
    val ____ptr = apply()
    (!____ptr).ref_count = ref_count
    (!____ptr).name = name
    (!____ptr).methods = methods
    (!____ptr).signals = signals
    (!____ptr).properties = properties
    (!____ptr).annotations = annotations
    ____ptr