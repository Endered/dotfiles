package bluetoothbatterywatcher
import libgio.GBusType.G_BUS_TYPE_SYSTEM
import libgio.GDBusCallFlags.G_DBUS_CALL_FLAGS_NONE
import libgio.GDBusProxy
import libgio.GDBusProxyFlags
import libgio.GDBusProxyFlags.G_DBUS_PROXY_FLAGS_NONE
import libgio.GVariant
import libgio.GVariantIter
import libgio.GVariantType
import libgio.g_dbus_proxy_call_sync
import libgio.g_dbus_proxy_new_for_bus_sync
import libgio.g_object_unref
import libgio.g_variant_classify
import libgio.g_variant_get
import libgio.g_variant_get_boolean
import libgio.g_variant_get_byte
import libgio.g_variant_get_child_value
import libgio.g_variant_get_int16
import libgio.g_variant_get_string
import libgio.g_variant_get_type
import libgio.g_variant_get_uint16
import libgio.g_variant_get_uint32
import libgio.g_variant_get_variant
import libgio.g_variant_iter_init
import libgio.g_variant_iter_next_value
import libgio.g_variant_n_children
import libgio.g_variant_new
import libgio.g_variant_type_element
import libgio.g_variant_type_equal
import libgio.g_variant_type_is_array
import libgio.g_variant_type_is_basic
import libgio.g_variant_type_is_tuple
import libgio.g_variant_type_new
import libgio.g_variant_type_peek_string
import libgio.g_variant_unref
import libgio.gchar
import libgio.gconstpointer
import libgio.gint
import libgio.gpointer
import libgio.gsize

import scala.scalanative.unsafe.CChar
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsafe.fromCString
import scala.scalanative.unsafe.given
import scala.scalanative.unsigned.UByte
import scala.scalanative.unsigned.UInt
import scala.scalanative.unsigned.UShort
import scala.scalanative.unsigned.given

extension (ptr: Ptr[CChar]) {
  inline def toGChar: Ptr[libgio.gchar] = ptr.asInstanceOf[Ptr[libgio.gchar]]
}
extension (ptr: Ptr[libgio.gchar]) {
  inline def toCString: Ptr[CChar] = ptr.asInstanceOf[Ptr[CChar]]
}

extension (n: Int) {
  inline def toGInt: gint = gint(n)
}

enum DbusVariant {
  // It does not fully implement the GVariant type.
  // Only implemented needed types.
  case DbusBoolean(value: Boolean)
  case DbusByte(value: Byte)
  case DbusUByte(value: UByte)
  case DbusString(value: String)
  case DbusObjectPath(value: String)
  case DbusArray(value: Seq[DbusVariant])
  case DbusTuple(value: Seq[DbusVariant])
  case DbusDictEntry(key: DbusVariant, value: DbusVariant)
  case DbusDict(value: Map[DbusVariant, DbusVariant])
  case DbusUInt(value: UInt)
  case DbusUShort(value: UShort)
  case DbusShort(value: Short)
  case DbusUnknown

  def minify: DbusVariant = this match {
    case DbusArray(value) =>
      if (value.forall(_.isInstanceOf[DbusDictEntry])) {
        DbusDict(
          value.map { case DbusDictEntry(key, value) =>
            key.minify -> value.minify
          }.toMap,
        )
      } else {
        DbusArray(
          value.map(_.minify),
        )
      }
    case DbusTuple(value) =>
      DbusTuple(value.map(_.minify))
    case c => c
  }

  def apply(i: Int) = {
    this match {
      case DbusArray(value) if i < value.length => value(i)
      case DbusTuple(value) if i < value.length => value(i)
      case x                                    => DbusUnknown
    }
  }

  def apply(key: DbusVariant) = {
    this match {
      case DbusDict(map) => map.getOrElse(key, DbusUnknown)
      case x             => x
    }
  }
}

def parseDbus(v: Ptr[GVariant]): DbusVariant = {
  import libgio.GVariantClass.*
  import scala.scalanative.unsafe.Zone
  import scala.scalanative.unsafe.alloc
  g_variant_classify(v) match {
    case G_VARIANT_CLASS_TUPLE =>
      val size = g_variant_n_children(v).value.toInt
      val res = (0 until size).map { i =>
        val element = g_variant_get_child_value(v, gsize(i.toUSize))
        val x = parseDbus(element)
        g_variant_unref(element)
        x
      }.toSeq
      DbusVariant.DbusTuple(res)
    case G_VARIANT_CLASS_ARRAY =>
      val size = g_variant_n_children(v).value.toInt
      val res = (0 until size).map { i =>
        val element = g_variant_get_child_value(v, gsize(i.toUSize))
        val x = parseDbus(element)
        g_variant_unref(element)
        x
      }.toSeq
      DbusVariant.DbusArray(res)
    case G_VARIANT_CLASS_DICT_ENTRY =>
      val key = g_variant_get_child_value(v, gsize(0.toUSize))
      val value = g_variant_get_child_value(v, gsize(1.toUSize))

      val res = DbusVariant.DbusDictEntry(
        key = parseDbus(key),
        value = parseDbus(value),
      )

      g_variant_unref(key)
      g_variant_unref(value)

      res

    case G_VARIANT_CLASS_STRING =>
      val value = fromCString(g_variant_get_string(v, null).toCString)

      DbusVariant.DbusString(value)
    case G_VARIANT_CLASS_OBJECT_PATH =>
      val value = fromCString(g_variant_get_string(v, null).toCString)

      DbusVariant.DbusObjectPath(value)

    case G_VARIANT_CLASS_VARIANT =>
      val value = g_variant_get_variant(v)
      val res = parseDbus(value)
      g_variant_unref(value)
      res

    case G_VARIANT_CLASS_UINT32 =>
      val value = g_variant_get_uint32(v)

      DbusVariant.DbusUInt(value.value)

    case G_VARIANT_CLASS_UINT16 =>
      val value = g_variant_get_uint16(v)

      DbusVariant.DbusUShort(value.value)

    case G_VARIANT_CLASS_BYTE =>
      val value = g_variant_get_byte(v)

      DbusVariant.DbusUByte(value.value)

    case G_VARIANT_CLASS_BOOLEAN =>
      val value = g_variant_get_boolean(v)

      DbusVariant.DbusBoolean(value.value != 0)

    case G_VARIANT_CLASS_INT16 =>
      val value = g_variant_get_int16(v)

      DbusVariant.DbusShort(value.value)

    case _ =>
      DbusVariant.DbusUnknown
  }
}

lazy val Icons = Map(
  "audio-headset" -> "H",
  "input-keyboard" -> "K",
  "computer" -> "C",
)

def run(args: Array[String]): Unit = {
  val dbusProxy: Ptr[GDBusProxy] = g_dbus_proxy_new_for_bus_sync(
    G_BUS_TYPE_SYSTEM,
    G_DBUS_PROXY_FLAGS_NONE,
    null,
    c"org.bluez".toGChar,
    c"/".toGChar,
    c"org.freedesktop.DBus.ObjectManager".toGChar,
    null,
    null,
  )

  if (dbusProxy == null) {
    throw new Error("Faild to get proxy object")
  }

  while (true) {
    val res: Ptr[GVariant] = g_dbus_proxy_call_sync(
      dbusProxy,
      c"GetManagedObjects".toGChar,
      g_variant_new(c"()".toGChar),
      G_DBUS_CALL_FLAGS_NONE,
      -1.toGInt,
      null,
      null,
    )

    if (res == null) {
      throw new Error("Failed to call method")
    }

    val parsed = parseDbus(res).minify

    val connectedDevices = parsed(0)
      .asInstanceOf[DbusVariant.DbusDict]
      .value
      .toSeq
      .flatMap { case (devicePath, value) =>
        val battery = value
          .apply(DbusVariant.DbusString("org.bluez.Battery1"))
          .apply(DbusVariant.DbusString("Percentage"))
        val icon = value
          .apply(DbusVariant.DbusString("org.bluez.Device1"))
          .apply(DbusVariant.DbusString("Icon"))

        Option.when(
          battery != DbusVariant.DbusUnknown &&
            icon != DbusVariant.DbusUnknown,
        ) {
          (
            devicePath.asInstanceOf[DbusVariant.DbusObjectPath].value,
            battery.asInstanceOf[DbusVariant.DbusUByte].value,
            icon.asInstanceOf[DbusVariant.DbusString].value,
          )
        }
      }
      .sortBy(_._1)

    val s = connectedDevices
      .map { case (_, battery, icon) =>
        val iconString = Icons.getOrElse(icon, "?")
        s"${iconString}:${battery}%"
      }
      .mkString(", ")

    println(s"BL ${s}")

    g_variant_unref(res)

    Thread.sleep(1000)
  }

  g_object_unref(dbusProxy.asInstanceOf[gpointer])
}
