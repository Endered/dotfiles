package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * GParameter: : the parameter value

 * [bindgen] header: /nix/store/gdq1i8iwvxjznbfnmg83lg5sgy35qp8i-glib-2.82.5-dev/include/glib-2.0/gobject/gparam.h
*/
opaque type GParameter = CStruct2[Ptr[gchar], GValue]

object GParameter:
  given _tag: Tag[GParameter] = Tag.materializeCStruct2Tag[Ptr[gchar], GValue]
  
  export fields.*
  private[libgio] object fields:
    extension (struct: GParameter)
      inline def name : Ptr[gchar] = struct._1
      inline def name_=(value: Ptr[gchar]): Unit = (!struct.at1 = value)
      inline def value : GValue = struct._2
      inline def value_=(value: GValue): Unit = (!struct.at2 = value)
    end extension
  
  // Allocates GParameter on the heap – fields are not initalised or zeroed out
  def apply()(using Zone): Ptr[GParameter] = scala.scalanative.unsafe.alloc[GParameter](1)
  def apply(name : Ptr[gchar], value : GValue)(using Zone): Ptr[GParameter] =
    val ____ptr = apply()
    (!____ptr).name = name
    (!____ptr).value = value
    ____ptr