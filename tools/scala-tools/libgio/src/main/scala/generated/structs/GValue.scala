package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GValue:

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gobject/gvalue.h
*/
opaque type GValue = CStruct2[GType, CArray[GValue_Data, Nat._2]]

object GValue:
  given _tag: Tag[GValue] = Tag.materializeCStruct2Tag[GType, CArray[GValue_Data, Nat._2]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GValue)
      inline def g_type : GType = struct._1
      inline def g_type_=(value: GType): Unit = (!struct.at1 = value)
      inline def data : CArray[GValue_Data, Nat._2] = struct._2
      inline def data_=(value: CArray[GValue_Data, Nat._2]): Unit = (!struct.at2 = value)
    end extension
  
  // Allocates GValue on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GValue] = scala.scalanative.unsafe.alloc[GValue](1)
  def apply(g_type : GType, data : CArray[GValue_Data, Nat._2])(using Zone): Ptr[GValue] =
    val ____ptr = apply()
    (!____ptr).g_type = g_type
    (!____ptr).data = data
    ____ptr
  
  
/**
 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gobject/gvalue.h
*/
opaque type GValue_Data = CArray[Byte, Nat._8]
object GValue_Data:
  given _tag: Tag[GValue_Data] = Tag.CArray[CChar, Nat._8](Tag.Byte, Tag.Nat8)
  
  def apply()(using Zone): Ptr[GValue_Data] =
    val ___ptr = _root_.scala.scalanative.unsafe.alloc[GValue_Data](1)
    ___ptr
  
  @scala.annotation.targetName("apply_v_int")
  def apply(v_int: gint)(using Zone): Ptr[GValue_Data] =
    val ___ptr = _root_.scala.scalanative.unsafe.alloc[GValue_Data](1)
    val un = !___ptr
    un.at(0).asInstanceOf[Ptr[gint]].update(0, v_int)
    ___ptr
  
  @scala.annotation.targetName("apply_v_uint")
  def apply(v_uint: guint)(using Zone): Ptr[GValue_Data] =
    val ___ptr = _root_.scala.scalanative.unsafe.alloc[GValue_Data](1)
    val un = !___ptr
    un.at(0).asInstanceOf[Ptr[guint]].update(0, v_uint)
    ___ptr
  
  @scala.annotation.targetName("apply_v_long")
  def apply(v_long: glong)(using Zone): Ptr[GValue_Data] =
    val ___ptr = _root_.scala.scalanative.unsafe.alloc[GValue_Data](1)
    val un = !___ptr
    un.at(0).asInstanceOf[Ptr[glong]].update(0, v_long)
    ___ptr
  
  @scala.annotation.targetName("apply_v_ulong")
  def apply(v_ulong: gulong)(using Zone): Ptr[GValue_Data] =
    val ___ptr = _root_.scala.scalanative.unsafe.alloc[GValue_Data](1)
    val un = !___ptr
    un.at(0).asInstanceOf[Ptr[gulong]].update(0, v_ulong)
    ___ptr
  
  @scala.annotation.targetName("apply_v_int64")
  def apply(v_int64: gint64)(using Zone): Ptr[GValue_Data] =
    val ___ptr = _root_.scala.scalanative.unsafe.alloc[GValue_Data](1)
    val un = !___ptr
    un.at(0).asInstanceOf[Ptr[gint64]].update(0, v_int64)
    ___ptr
  
  @scala.annotation.targetName("apply_v_uint64")
  def apply(v_uint64: guint64)(using Zone): Ptr[GValue_Data] =
    val ___ptr = _root_.scala.scalanative.unsafe.alloc[GValue_Data](1)
    val un = !___ptr
    un.at(0).asInstanceOf[Ptr[guint64]].update(0, v_uint64)
    ___ptr
  
  @scala.annotation.targetName("apply_v_float")
  def apply(v_float: gfloat)(using Zone): Ptr[GValue_Data] =
    val ___ptr = _root_.scala.scalanative.unsafe.alloc[GValue_Data](1)
    val un = !___ptr
    un.at(0).asInstanceOf[Ptr[gfloat]].update(0, v_float)
    ___ptr
  
  @scala.annotation.targetName("apply_v_double")
  def apply(v_double: gdouble)(using Zone): Ptr[GValue_Data] =
    val ___ptr = _root_.scala.scalanative.unsafe.alloc[GValue_Data](1)
    val un = !___ptr
    un.at(0).asInstanceOf[Ptr[gdouble]].update(0, v_double)
    ___ptr
  
  @scala.annotation.targetName("apply_v_pointer")
  def apply(v_pointer: gpointer)(using Zone): Ptr[GValue_Data] =
    val ___ptr = _root_.scala.scalanative.unsafe.alloc[GValue_Data](1)
    val un = !___ptr
    un.at(0).asInstanceOf[Ptr[gpointer]].update(0, v_pointer)
    ___ptr
  
  extension (struct: GValue_Data)
    inline def v_int : gint = !struct.at(0).asInstanceOf[Ptr[gint]]
    inline def v_int_=(value: gint): Unit = !struct.at(0).asInstanceOf[Ptr[gint]] = value
    inline def v_uint : guint = !struct.at(0).asInstanceOf[Ptr[guint]]
    inline def v_uint_=(value: guint): Unit = !struct.at(0).asInstanceOf[Ptr[guint]] = value
    inline def v_long : glong = !struct.at(0).asInstanceOf[Ptr[glong]]
    inline def v_long_=(value: glong): Unit = !struct.at(0).asInstanceOf[Ptr[glong]] = value
    inline def v_ulong : gulong = !struct.at(0).asInstanceOf[Ptr[gulong]]
    inline def v_ulong_=(value: gulong): Unit = !struct.at(0).asInstanceOf[Ptr[gulong]] = value
    inline def v_int64 : gint64 = !struct.at(0).asInstanceOf[Ptr[gint64]]
    inline def v_int64_=(value: gint64): Unit = !struct.at(0).asInstanceOf[Ptr[gint64]] = value
    inline def v_uint64 : guint64 = !struct.at(0).asInstanceOf[Ptr[guint64]]
    inline def v_uint64_=(value: guint64): Unit = !struct.at(0).asInstanceOf[Ptr[guint64]] = value
    inline def v_float : gfloat = !struct.at(0).asInstanceOf[Ptr[gfloat]]
    inline def v_float_=(value: gfloat): Unit = !struct.at(0).asInstanceOf[Ptr[gfloat]] = value
    inline def v_double : gdouble = !struct.at(0).asInstanceOf[Ptr[gdouble]]
    inline def v_double_=(value: gdouble): Unit = !struct.at(0).asInstanceOf[Ptr[gdouble]] = value
    inline def v_pointer : gpointer = !struct.at(0).asInstanceOf[Ptr[gpointer]]
    inline def v_pointer_=(value: gpointer): Unit = !struct.at(0).asInstanceOf[Ptr[gpointer]] = value