package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gobject/gclosure.h
*/
opaque type GClosure = CStruct13[guint, guint, guint, guint, guint, guint, guint, guint, guint, guint, CFuncPtr6[Ptr[Byte], Ptr[GValue], guint, Ptr[GValue], gpointer, gpointer, Unit], gpointer, Ptr[Byte]]

object GClosure:
  given _tag: Tag[GClosure] = Tag.materializeCStruct13Tag[guint, guint, guint, guint, guint, guint, guint, guint, guint, guint, CFuncPtr6[Ptr[Byte], Ptr[GValue], guint, Ptr[GValue], gpointer, gpointer, Unit], gpointer, Ptr[Byte]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GClosure)
      inline def ref_count : guint = struct._1
      inline def ref_count_=(value: guint): Unit = (!struct.at1 = value)
      inline def meta_marshal_nouse : guint = struct._2
      inline def meta_marshal_nouse_=(value: guint): Unit = (!struct.at2 = value)
      inline def n_guards : guint = struct._3
      inline def n_guards_=(value: guint): Unit = (!struct.at3 = value)
      inline def n_fnotifiers : guint = struct._4
      inline def n_fnotifiers_=(value: guint): Unit = (!struct.at4 = value)
      inline def n_inotifiers : guint = struct._5
      inline def n_inotifiers_=(value: guint): Unit = (!struct.at5 = value)
      inline def in_inotify : guint = struct._6
      inline def in_inotify_=(value: guint): Unit = (!struct.at6 = value)
      inline def floating : guint = struct._7
      inline def floating_=(value: guint): Unit = (!struct.at7 = value)
      inline def derivative_flag : guint = struct._8
      inline def derivative_flag_=(value: guint): Unit = (!struct.at8 = value)
      inline def in_marshal : guint = struct._9
      inline def in_marshal_=(value: guint): Unit = (!struct.at9 = value)
      inline def is_invalid : guint = struct._10
      inline def is_invalid_=(value: guint): Unit = (!struct.at10 = value)
      inline def marshal : CFuncPtr6[Ptr[GClosure], Ptr[GValue], guint, Ptr[GValue], gpointer, gpointer, Unit] = struct._11.asInstanceOf[CFuncPtr6[Ptr[GClosure], Ptr[GValue], guint, Ptr[GValue], gpointer, gpointer, Unit]]
      inline def marshal_=(value: CFuncPtr6[Ptr[GClosure], Ptr[GValue], guint, Ptr[GValue], gpointer, gpointer, Unit]): Unit = (!struct.at11 = value.asInstanceOf[CFuncPtr6[Ptr[Byte], Ptr[GValue], guint, Ptr[GValue], gpointer, gpointer, Unit]])
      inline def data : gpointer = struct._12
      inline def data_=(value: gpointer): Unit = (!struct.at12 = value)
      inline def notifiers : Ptr[GClosureNotifyData] = struct._13.asInstanceOf[Ptr[GClosureNotifyData]]
      inline def notifiers_=(value: Ptr[GClosureNotifyData]): Unit = (!struct.at13 = value.asInstanceOf[Ptr[Byte]])
    end extension
  
  // Allocates GClosure on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GClosure] = scala.scalanative.unsafe.alloc[GClosure](1)
  def apply(ref_count : guint, meta_marshal_nouse : guint, n_guards : guint, n_fnotifiers : guint, n_inotifiers : guint, in_inotify : guint, floating : guint, derivative_flag : guint, in_marshal : guint, is_invalid : guint, marshal : CFuncPtr6[Ptr[GClosure], Ptr[GValue], guint, Ptr[GValue], gpointer, gpointer, Unit], data : gpointer, notifiers : Ptr[GClosureNotifyData])(using Zone): Ptr[GClosure] =
    val ____ptr = apply()
    (!____ptr).ref_count = ref_count
    (!____ptr).meta_marshal_nouse = meta_marshal_nouse
    (!____ptr).n_guards = n_guards
    (!____ptr).n_fnotifiers = n_fnotifiers
    (!____ptr).n_inotifiers = n_inotifiers
    (!____ptr).in_inotify = in_inotify
    (!____ptr).floating = floating
    (!____ptr).derivative_flag = derivative_flag
    (!____ptr).in_marshal = in_marshal
    (!____ptr).is_invalid = is_invalid
    (!____ptr).marshal = marshal
    (!____ptr).data = data
    (!____ptr).notifiers = notifiers
    ____ptr