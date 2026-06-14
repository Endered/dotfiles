package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GDBusNodeInfo: _count: The reference count or -1 if statically allocated. : The path of the node or %NULL if omitted. Note that this may be a relative path. See the D-Bus specification for more details. : (array zero-terminated=1): A pointer to a %NULL-terminated array of pointers to #GDBusNodeInfo structures or %NULL if there are no nodes. : (array zero-terminated=1): A pointer to a %NULL-terminated array of pointers to #GDBusAnnotationInfo structures or %NULL if there are no annotations.

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/gdbusintrospection.h
*/
opaque type GDBusNodeInfo = CStruct5[gint, Ptr[gchar], Ptr[Byte], Ptr[Byte], Ptr[Byte]]

object GDBusNodeInfo:
  given _tag: Tag[GDBusNodeInfo] = Tag.materializeCStruct5Tag[gint, Ptr[gchar], Ptr[Byte], Ptr[Byte], Ptr[Byte]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GDBusNodeInfo)
      inline def ref_count : gint = struct._1
      inline def ref_count_=(value: gint): Unit = (!struct.at1 = value)
      inline def path : Ptr[gchar] = struct._2
      inline def path_=(value: Ptr[gchar]): Unit = (!struct.at2 = value)
      inline def interfaces : Ptr[Ptr[GDBusInterfaceInfo]] = struct._3.asInstanceOf[Ptr[Ptr[GDBusInterfaceInfo]]]
      inline def interfaces_=(value: Ptr[Ptr[GDBusInterfaceInfo]]): Unit = (!struct.at3 = value.asInstanceOf[Ptr[Byte]])
      inline def nodes : Ptr[Ptr[GDBusNodeInfo]] = struct._4.asInstanceOf[Ptr[Ptr[GDBusNodeInfo]]]
      inline def nodes_=(value: Ptr[Ptr[GDBusNodeInfo]]): Unit = (!struct.at4 = value.asInstanceOf[Ptr[Byte]])
      inline def annotations : Ptr[Ptr[GDBusAnnotationInfo]] = struct._5.asInstanceOf[Ptr[Ptr[GDBusAnnotationInfo]]]
      inline def annotations_=(value: Ptr[Ptr[GDBusAnnotationInfo]]): Unit = (!struct.at5 = value.asInstanceOf[Ptr[Byte]])
    end extension
  
  // Allocates GDBusNodeInfo on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GDBusNodeInfo] = scala.scalanative.unsafe.alloc[GDBusNodeInfo](1)
  def apply(ref_count : gint, path : Ptr[gchar], interfaces : Ptr[Ptr[GDBusInterfaceInfo]], nodes : Ptr[Ptr[GDBusNodeInfo]], annotations : Ptr[Ptr[GDBusAnnotationInfo]])(using Zone): Ptr[GDBusNodeInfo] =
    val ____ptr = apply()
    (!____ptr).ref_count = ref_count
    (!____ptr).path = path
    (!____ptr).interfaces = interfaces
    (!____ptr).nodes = nodes
    (!____ptr).annotations = annotations
    ____ptr