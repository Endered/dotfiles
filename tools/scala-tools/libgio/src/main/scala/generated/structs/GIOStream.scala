package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gio/giostream.h
*/
opaque type GIOStream = CStruct2[GObject, Ptr[GIOStreamPrivate]]

object GIOStream:
  given _tag: Tag[GIOStream] = Tag.materializeCStruct2Tag[GObject, Ptr[GIOStreamPrivate]]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GIOStream)
      inline def parent_instance : GObject = struct._1
      inline def parent_instance_=(value: GObject): Unit = (!struct.at1 = value)
      inline def priv : Ptr[GIOStreamPrivate] = struct._2
      inline def priv_=(value: Ptr[GIOStreamPrivate]): Unit = (!struct.at2 = value)
    end extension
  
  // Allocates GIOStream on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GIOStream] = scala.scalanative.unsafe.alloc[GIOStream](1)
  def apply(parent_instance : GObject, priv : Ptr[GIOStreamPrivate])(using Zone): Ptr[GIOStream] =
    val ____ptr = apply()
    (!____ptr).parent_instance = parent_instance
    (!____ptr).priv = priv
    ____ptr