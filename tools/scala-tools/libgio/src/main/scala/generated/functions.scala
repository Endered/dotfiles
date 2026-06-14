package libgio

// This file was generated using sn-bindgen 0.4.4: https://sn-bindgen.indoorvivants.com/

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.libc.*
import _root_.scala.scalanative.*

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_action_group_get(connection : Ptr[GDBusConnection], bus_name : Ptr[gchar], object_path : Ptr[gchar]): Ptr[GDBusActionGroup] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_action_group_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_address_escape_value(string : Ptr[gchar]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_address_get_for_bus_sync(bus_type : GBusType, cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_address_get_stream(address : Ptr[gchar], cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_address_get_stream_finish(res : Ptr[GAsyncResult], out_guid : Ptr[Ptr[gchar]], error : Ptr[Ptr[GError]]): Ptr[GIOStream] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_address_get_stream_sync(address : Ptr[gchar], out_guid : Ptr[Ptr[gchar]], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GIOStream] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_annotation_info_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_annotation_info_lookup(annotations : Ptr[Ptr[GDBusAnnotationInfo]], name : Ptr[gchar]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_annotation_info_ref(info : Ptr[GDBusAnnotationInfo]): Ptr[GDBusAnnotationInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_annotation_info_unref(info : Ptr[GDBusAnnotationInfo]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_arg_info_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_arg_info_ref(info : Ptr[GDBusArgInfo]): Ptr[GDBusArgInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_arg_info_unref(info : Ptr[GDBusArgInfo]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_auth_observer_allow_mechanism(observer : Ptr[GDBusAuthObserver], mechanism : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_auth_observer_authorize_authenticated_peer(observer : Ptr[GDBusAuthObserver], stream : Ptr[GIOStream], credentials : Ptr[GCredentials]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_auth_observer_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_auth_observer_new(): Ptr[GDBusAuthObserver] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_call_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_capability_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_add_filter(connection : Ptr[GDBusConnection], filter_function : GDBusMessageFilterFunction, user_data : gpointer, user_data_free_func : GDestroyNotify): guint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_call(connection : Ptr[GDBusConnection], bus_name : Ptr[gchar], object_path : Ptr[gchar], interface_name : Ptr[gchar], method_name : Ptr[gchar], parameters : Ptr[GVariant], reply_type : Ptr[GVariantType], flags : GDBusCallFlags, timeout_msec : gint, cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_call_finish(connection : Ptr[GDBusConnection], res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_call_sync(connection : Ptr[GDBusConnection], bus_name : Ptr[gchar], object_path : Ptr[gchar], interface_name : Ptr[gchar], method_name : Ptr[gchar], parameters : Ptr[GVariant], reply_type : Ptr[GVariantType], flags : GDBusCallFlags, timeout_msec : gint, cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_call_with_unix_fd_list(connection : Ptr[GDBusConnection], bus_name : Ptr[gchar], object_path : Ptr[gchar], interface_name : Ptr[gchar], method_name : Ptr[gchar], parameters : Ptr[GVariant], reply_type : Ptr[GVariantType], flags : GDBusCallFlags, timeout_msec : gint, fd_list : Ptr[GUnixFDList], cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_call_with_unix_fd_list_finish(connection : Ptr[GDBusConnection], out_fd_list : Ptr[Ptr[GUnixFDList]], res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_call_with_unix_fd_list_sync(connection : Ptr[GDBusConnection], bus_name : Ptr[gchar], object_path : Ptr[gchar], interface_name : Ptr[gchar], method_name : Ptr[gchar], parameters : Ptr[GVariant], reply_type : Ptr[GVariantType], flags : GDBusCallFlags, timeout_msec : gint, fd_list : Ptr[GUnixFDList], out_fd_list : Ptr[Ptr[GUnixFDList]], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_close(connection : Ptr[GDBusConnection], cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_close_finish(connection : Ptr[GDBusConnection], res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_close_sync(connection : Ptr[GDBusConnection], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_emit_signal(connection : Ptr[GDBusConnection], destination_bus_name : Ptr[gchar], object_path : Ptr[gchar], interface_name : Ptr[gchar], signal_name : Ptr[gchar], parameters : Ptr[GVariant], error : Ptr[Ptr[GError]]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_export_action_group(connection : Ptr[GDBusConnection], object_path : Ptr[gchar], action_group : Ptr[GActionGroup], error : Ptr[Ptr[GError]]): guint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_export_menu_model(connection : Ptr[GDBusConnection], object_path : Ptr[gchar], menu : Ptr[GMenuModel], error : Ptr[Ptr[GError]]): guint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_flush(connection : Ptr[GDBusConnection], cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_flush_finish(connection : Ptr[GDBusConnection], res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_flush_sync(connection : Ptr[GDBusConnection], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_get_capabilities(connection : Ptr[GDBusConnection]): GDBusCapabilityFlags = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_get_exit_on_close(connection : Ptr[GDBusConnection]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_get_flags(connection : Ptr[GDBusConnection]): GDBusConnectionFlags = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_get_guid(connection : Ptr[GDBusConnection]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_get_last_serial(connection : Ptr[GDBusConnection]): guint32 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_get_peer_credentials(connection : Ptr[GDBusConnection]): Ptr[GCredentials] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_get_stream(connection : Ptr[GDBusConnection]): Ptr[GIOStream] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_get_unique_name(connection : Ptr[GDBusConnection]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_is_closed(connection : Ptr[GDBusConnection]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_new(stream : Ptr[GIOStream], guid : Ptr[gchar], flags : GDBusConnectionFlags, observer : Ptr[GDBusAuthObserver], cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_new_finish(res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GDBusConnection] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_new_for_address(address : Ptr[gchar], flags : GDBusConnectionFlags, observer : Ptr[GDBusAuthObserver], cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_new_for_address_finish(res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GDBusConnection] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_new_for_address_sync(address : Ptr[gchar], flags : GDBusConnectionFlags, observer : Ptr[GDBusAuthObserver], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GDBusConnection] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_new_sync(stream : Ptr[GIOStream], guid : Ptr[gchar], flags : GDBusConnectionFlags, observer : Ptr[GDBusAuthObserver], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GDBusConnection] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_register_object(connection : Ptr[GDBusConnection], object_path : Ptr[gchar], interface_info : Ptr[GDBusInterfaceInfo], vtable : Ptr[GDBusInterfaceVTable], user_data : gpointer, user_data_free_func : GDestroyNotify, error : Ptr[Ptr[GError]]): guint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_register_object_with_closures(connection : Ptr[GDBusConnection], object_path : Ptr[gchar], interface_info : Ptr[GDBusInterfaceInfo], method_call_closure : Ptr[GClosure], get_property_closure : Ptr[GClosure], set_property_closure : Ptr[GClosure], error : Ptr[Ptr[GError]]): guint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_register_subtree(connection : Ptr[GDBusConnection], object_path : Ptr[gchar], vtable : Ptr[GDBusSubtreeVTable], flags : GDBusSubtreeFlags, user_data : gpointer, user_data_free_func : GDestroyNotify, error : Ptr[Ptr[GError]]): guint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_remove_filter(connection : Ptr[GDBusConnection], filter_id : guint): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_send_message(connection : Ptr[GDBusConnection], message : Ptr[GDBusMessage], flags : GDBusSendMessageFlags, out_serial : Ptr[guint32], error : Ptr[Ptr[GError]]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_send_message_with_reply(connection : Ptr[GDBusConnection], message : Ptr[GDBusMessage], flags : GDBusSendMessageFlags, timeout_msec : gint, out_serial : Ptr[guint32], cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_send_message_with_reply_finish(connection : Ptr[GDBusConnection], res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_send_message_with_reply_sync(connection : Ptr[GDBusConnection], message : Ptr[GDBusMessage], flags : GDBusSendMessageFlags, timeout_msec : gint, out_serial : Ptr[guint32], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_set_exit_on_close(connection : Ptr[GDBusConnection], exit_on_close : gboolean): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_signal_subscribe(connection : Ptr[GDBusConnection], sender : Ptr[gchar], interface_name : Ptr[gchar], member : Ptr[gchar], object_path : Ptr[gchar], arg0 : Ptr[gchar], flags : GDBusSignalFlags, callback : GDBusSignalCallback, user_data : gpointer, user_data_free_func : GDestroyNotify): guint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_signal_unsubscribe(connection : Ptr[GDBusConnection], subscription_id : guint): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_start_message_processing(connection : Ptr[GDBusConnection]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_unexport_action_group(connection : Ptr[GDBusConnection], export_id : guint): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_unexport_menu_model(connection : Ptr[GDBusConnection], export_id : guint): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_unregister_object(connection : Ptr[GDBusConnection], registration_id : guint): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_connection_unregister_subtree(connection : Ptr[GDBusConnection], registration_id : guint): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_encode_gerror(error : Ptr[GError]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_get_remote_error(error : Ptr[GError]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_is_remote_error(error : Ptr[GError]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_new_for_dbus_error(dbus_error_name : Ptr[gchar], dbus_error_message : Ptr[gchar]): Ptr[GError] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_quark(): GQuark = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_register_error(error_domain : GQuark, error_code : gint, dbus_error_name : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_register_error_domain(error_domain_quark_name : Ptr[gchar], quark_volatile : Ptr[gsize], entries : Ptr[GDBusErrorEntry], num_entries : guint): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_set_dbus_error(error : Ptr[Ptr[GError]], dbus_error_name : Ptr[gchar], dbus_error_message : Ptr[gchar], format : Ptr[gchar], rest: Any*): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_set_dbus_error_valist(error : Ptr[Ptr[GError]], dbus_error_name : Ptr[gchar], dbus_error_message : Ptr[gchar], format : Ptr[gchar], var_args : va_list): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_strip_remote_error(error : Ptr[GError]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_error_unregister_error(error_domain : GQuark, error_code : gint, dbus_error_name : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_escape_object_path(s : Ptr[gchar]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_escape_object_path_bytestring(bytes : Ptr[guint8]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_generate_guid(): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_gvalue_to_gvariant(gvalue : Ptr[GValue], `type` : Ptr[GVariantType]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_gvariant_to_gvalue(value : Ptr[GVariant], out_gvalue : Ptr[GValue]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_dup_object(`interface_` : Ptr[GDBusInterface]): Ptr[GDBusObject] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_get_info(`interface_` : Ptr[GDBusInterface]): Ptr[GDBusInterfaceInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_get_object(`interface_` : Ptr[GDBusInterface]): Ptr[GDBusObject] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_info_cache_build(info : Ptr[GDBusInterfaceInfo]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_info_cache_release(info : Ptr[GDBusInterfaceInfo]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_info_generate_xml(info : Ptr[GDBusInterfaceInfo], indent : guint, string_builder : Ptr[GString]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_info_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_info_lookup_method(info : Ptr[GDBusInterfaceInfo], name : Ptr[gchar]): Ptr[GDBusMethodInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_info_lookup_property(info : Ptr[GDBusInterfaceInfo], name : Ptr[gchar]): Ptr[GDBusPropertyInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_info_lookup_signal(info : Ptr[GDBusInterfaceInfo], name : Ptr[gchar]): Ptr[GDBusSignalInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_info_ref(info : Ptr[GDBusInterfaceInfo]): Ptr[GDBusInterfaceInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_info_unref(info : Ptr[GDBusInterfaceInfo]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_set_object(`interface_` : Ptr[GDBusInterface], `object` : Ptr[GDBusObject]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_export(`interface_` : Ptr[GDBusInterfaceSkeleton], connection : Ptr[GDBusConnection], object_path : Ptr[gchar], error : Ptr[Ptr[GError]]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_flush(`interface_` : Ptr[GDBusInterfaceSkeleton]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_get_connection(`interface_` : Ptr[GDBusInterfaceSkeleton]): Ptr[GDBusConnection] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_get_connections(`interface_` : Ptr[GDBusInterfaceSkeleton]): Ptr[GList] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_get_flags(`interface_` : Ptr[GDBusInterfaceSkeleton]): GDBusInterfaceSkeletonFlags = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_get_info(`interface_` : Ptr[GDBusInterfaceSkeleton]): Ptr[GDBusInterfaceInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_get_object_path(`interface_` : Ptr[GDBusInterfaceSkeleton]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_get_properties(`interface_` : Ptr[GDBusInterfaceSkeleton]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_get_vtable(`interface_` : Ptr[GDBusInterfaceSkeleton]): Ptr[GDBusInterfaceVTable] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_has_connection(`interface_` : Ptr[GDBusInterfaceSkeleton], connection : Ptr[GDBusConnection]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_set_flags(`interface_` : Ptr[GDBusInterfaceSkeleton], flags : GDBusInterfaceSkeletonFlags): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_unexport(`interface_` : Ptr[GDBusInterfaceSkeleton]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_interface_skeleton_unexport_from_connection(`interface_` : Ptr[GDBusInterfaceSkeleton], connection : Ptr[GDBusConnection]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_is_address(string : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_is_error_name(string : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_is_guid(string : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_is_interface_name(string : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_is_member_name(string : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_is_name(string : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_is_supported_address(string : Ptr[gchar], error : Ptr[Ptr[GError]]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_is_unique_name(string : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_menu_model_get(connection : Ptr[GDBusConnection], bus_name : Ptr[gchar], object_path : Ptr[gchar]): Ptr[GDBusMenuModel] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_menu_model_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_byte_order_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_bytes_needed(blob : Ptr[guchar], blob_len : gsize, error : Ptr[Ptr[GError]]): gssize = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_copy(message : Ptr[GDBusMessage], error : Ptr[Ptr[GError]]): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_arg0(message : Ptr[GDBusMessage]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_arg0_path(message : Ptr[GDBusMessage]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_body(message : Ptr[GDBusMessage]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_byte_order(message : Ptr[GDBusMessage]): GDBusMessageByteOrder = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_destination(message : Ptr[GDBusMessage]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_error_name(message : Ptr[GDBusMessage]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_flags(message : Ptr[GDBusMessage]): GDBusMessageFlags = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_header(message : Ptr[GDBusMessage], header_field : GDBusMessageHeaderField): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_header_fields(message : Ptr[GDBusMessage]): Ptr[guchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_interface(message : Ptr[GDBusMessage]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_locked(message : Ptr[GDBusMessage]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_member(message : Ptr[GDBusMessage]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_message_type(message : Ptr[GDBusMessage]): GDBusMessageType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_num_unix_fds(message : Ptr[GDBusMessage]): guint32 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_path(message : Ptr[GDBusMessage]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_reply_serial(message : Ptr[GDBusMessage]): guint32 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_sender(message : Ptr[GDBusMessage]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_serial(message : Ptr[GDBusMessage]): guint32 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_signature(message : Ptr[GDBusMessage]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_get_unix_fd_list(message : Ptr[GDBusMessage]): Ptr[GUnixFDList] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_header_field_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_lock(message : Ptr[GDBusMessage]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_new(): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_new_from_blob(blob : Ptr[guchar], blob_len : gsize, capabilities : GDBusCapabilityFlags, error : Ptr[Ptr[GError]]): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_new_method_call(name : Ptr[gchar], path : Ptr[gchar], `interface_` : Ptr[gchar], method : Ptr[gchar]): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_new_method_error(method_call_message : Ptr[GDBusMessage], error_name : Ptr[gchar], error_message_format : Ptr[gchar], rest: Any*): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_new_method_error_literal(method_call_message : Ptr[GDBusMessage], error_name : Ptr[gchar], error_message : Ptr[gchar]): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_new_method_error_valist(method_call_message : Ptr[GDBusMessage], error_name : Ptr[gchar], error_message_format : Ptr[gchar], var_args : va_list): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_new_method_reply(method_call_message : Ptr[GDBusMessage]): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_new_signal(path : Ptr[gchar], `interface_` : Ptr[gchar], signal : Ptr[gchar]): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_print(message : Ptr[GDBusMessage], indent : guint): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_body(message : Ptr[GDBusMessage], body : Ptr[GVariant]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_byte_order(message : Ptr[GDBusMessage], byte_order : GDBusMessageByteOrder): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_destination(message : Ptr[GDBusMessage], value : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_error_name(message : Ptr[GDBusMessage], value : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_flags(message : Ptr[GDBusMessage], flags : GDBusMessageFlags): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_header(message : Ptr[GDBusMessage], header_field : GDBusMessageHeaderField, value : Ptr[GVariant]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_interface(message : Ptr[GDBusMessage], value : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_member(message : Ptr[GDBusMessage], value : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_message_type(message : Ptr[GDBusMessage], `type` : GDBusMessageType): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_num_unix_fds(message : Ptr[GDBusMessage], value : guint32): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_path(message : Ptr[GDBusMessage], value : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_reply_serial(message : Ptr[GDBusMessage], value : guint32): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_sender(message : Ptr[GDBusMessage], value : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_serial(message : Ptr[GDBusMessage], serial : guint32): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_signature(message : Ptr[GDBusMessage], value : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_set_unix_fd_list(message : Ptr[GDBusMessage], fd_list : Ptr[GUnixFDList]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_to_blob(message : Ptr[GDBusMessage], out_size : Ptr[gsize], capabilities : GDBusCapabilityFlags, error : Ptr[Ptr[GError]]): Ptr[guchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_to_gerror(message : Ptr[GDBusMessage], error : Ptr[Ptr[GError]]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_message_type_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_info_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_info_ref(info : Ptr[GDBusMethodInfo]): Ptr[GDBusMethodInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_info_unref(info : Ptr[GDBusMethodInfo]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_connection(invocation : Ptr[GDBusMethodInvocation]): Ptr[GDBusConnection] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_interface_name(invocation : Ptr[GDBusMethodInvocation]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_message(invocation : Ptr[GDBusMethodInvocation]): Ptr[GDBusMessage] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_method_info(invocation : Ptr[GDBusMethodInvocation]): Ptr[GDBusMethodInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_method_name(invocation : Ptr[GDBusMethodInvocation]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_object_path(invocation : Ptr[GDBusMethodInvocation]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_parameters(invocation : Ptr[GDBusMethodInvocation]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_property_info(invocation : Ptr[GDBusMethodInvocation]): Ptr[GDBusPropertyInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_sender(invocation : Ptr[GDBusMethodInvocation]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_get_user_data(invocation : Ptr[GDBusMethodInvocation]): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_return_dbus_error(invocation : Ptr[GDBusMethodInvocation], error_name : Ptr[gchar], error_message : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_return_error(invocation : Ptr[GDBusMethodInvocation], domain : GQuark, code : gint, format : Ptr[gchar], rest: Any*): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_return_error_literal(invocation : Ptr[GDBusMethodInvocation], domain : GQuark, code : gint, message : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_return_error_valist(invocation : Ptr[GDBusMethodInvocation], domain : GQuark, code : gint, format : Ptr[gchar], var_args : va_list): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_return_gerror(invocation : Ptr[GDBusMethodInvocation], error : Ptr[GError]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_return_value(invocation : Ptr[GDBusMethodInvocation], parameters : Ptr[GVariant]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_return_value_with_unix_fd_list(invocation : Ptr[GDBusMethodInvocation], parameters : Ptr[GVariant], fd_list : Ptr[GUnixFDList]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_method_invocation_take_error(invocation : Ptr[GDBusMethodInvocation], error : Ptr[GError]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_node_info_generate_xml(info : Ptr[GDBusNodeInfo], indent : guint, string_builder : Ptr[GString]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_node_info_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_node_info_lookup_interface(info : Ptr[GDBusNodeInfo], name : Ptr[gchar]): Ptr[GDBusInterfaceInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_node_info_new_for_xml(xml_data : Ptr[gchar], error : Ptr[Ptr[GError]]): Ptr[GDBusNodeInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_node_info_ref(info : Ptr[GDBusNodeInfo]): Ptr[GDBusNodeInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_node_info_unref(info : Ptr[GDBusNodeInfo]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_get_interface(`object` : Ptr[GDBusObject], interface_name : Ptr[gchar]): Ptr[GDBusInterface] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_get_interfaces(`object` : Ptr[GDBusObject]): Ptr[GList] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_get_object_path(`object` : Ptr[GDBusObject]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_get_connection(manager : Ptr[GDBusObjectManagerClient]): Ptr[GDBusConnection] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_get_flags(manager : Ptr[GDBusObjectManagerClient]): GDBusObjectManagerClientFlags = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_get_name(manager : Ptr[GDBusObjectManagerClient]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_get_name_owner(manager : Ptr[GDBusObjectManagerClient]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_new(connection : Ptr[GDBusConnection], flags : GDBusObjectManagerClientFlags, name : Ptr[gchar], object_path : Ptr[gchar], get_proxy_type_func : GDBusProxyTypeFunc, get_proxy_type_user_data : gpointer, get_proxy_type_destroy_notify : GDestroyNotify, cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_new_finish(res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GDBusObjectManager] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_new_for_bus(bus_type : GBusType, flags : GDBusObjectManagerClientFlags, name : Ptr[gchar], object_path : Ptr[gchar], get_proxy_type_func : GDBusProxyTypeFunc, get_proxy_type_user_data : gpointer, get_proxy_type_destroy_notify : GDestroyNotify, cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_new_for_bus_finish(res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GDBusObjectManager] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_new_for_bus_sync(bus_type : GBusType, flags : GDBusObjectManagerClientFlags, name : Ptr[gchar], object_path : Ptr[gchar], get_proxy_type_func : GDBusProxyTypeFunc, get_proxy_type_user_data : gpointer, get_proxy_type_destroy_notify : GDestroyNotify, cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GDBusObjectManager] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_client_new_sync(connection : Ptr[GDBusConnection], flags : GDBusObjectManagerClientFlags, name : Ptr[gchar], object_path : Ptr[gchar], get_proxy_type_func : GDBusProxyTypeFunc, get_proxy_type_user_data : gpointer, get_proxy_type_destroy_notify : GDestroyNotify, cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GDBusObjectManager] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_get_interface(manager : Ptr[GDBusObjectManager], object_path : Ptr[gchar], interface_name : Ptr[gchar]): Ptr[GDBusInterface] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_get_object(manager : Ptr[GDBusObjectManager], object_path : Ptr[gchar]): Ptr[GDBusObject] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_get_object_path(manager : Ptr[GDBusObjectManager]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_get_objects(manager : Ptr[GDBusObjectManager]): Ptr[GList] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_server_export(manager : Ptr[GDBusObjectManagerServer], `object` : Ptr[GDBusObjectSkeleton]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_server_export_uniquely(manager : Ptr[GDBusObjectManagerServer], `object` : Ptr[GDBusObjectSkeleton]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_server_get_connection(manager : Ptr[GDBusObjectManagerServer]): Ptr[GDBusConnection] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_server_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_server_is_exported(manager : Ptr[GDBusObjectManagerServer], `object` : Ptr[GDBusObjectSkeleton]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_server_new(object_path : Ptr[gchar]): Ptr[GDBusObjectManagerServer] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_server_set_connection(manager : Ptr[GDBusObjectManagerServer], connection : Ptr[GDBusConnection]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_manager_server_unexport(manager : Ptr[GDBusObjectManagerServer], object_path : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_proxy_get_connection(proxy : Ptr[GDBusObjectProxy]): Ptr[GDBusConnection] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_proxy_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_proxy_new(connection : Ptr[GDBusConnection], object_path : Ptr[gchar]): Ptr[GDBusObjectProxy] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_skeleton_add_interface(`object` : Ptr[GDBusObjectSkeleton], `interface_` : Ptr[GDBusInterfaceSkeleton]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_skeleton_flush(`object` : Ptr[GDBusObjectSkeleton]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_skeleton_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_skeleton_new(object_path : Ptr[gchar]): Ptr[GDBusObjectSkeleton] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_skeleton_remove_interface(`object` : Ptr[GDBusObjectSkeleton], `interface_` : Ptr[GDBusInterfaceSkeleton]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_skeleton_remove_interface_by_name(`object` : Ptr[GDBusObjectSkeleton], interface_name : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_object_skeleton_set_object_path(`object` : Ptr[GDBusObjectSkeleton], object_path : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_property_info_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_property_info_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_property_info_ref(info : Ptr[GDBusPropertyInfo]): Ptr[GDBusPropertyInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_property_info_unref(info : Ptr[GDBusPropertyInfo]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_call(proxy : Ptr[GDBusProxy], method_name : Ptr[gchar], parameters : Ptr[GVariant], flags : GDBusCallFlags, timeout_msec : gint, cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_call_finish(proxy : Ptr[GDBusProxy], res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_call_sync(proxy : Ptr[GDBusProxy], method_name : Ptr[gchar], parameters : Ptr[GVariant], flags : GDBusCallFlags, timeout_msec : gint, cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_call_with_unix_fd_list(proxy : Ptr[GDBusProxy], method_name : Ptr[gchar], parameters : Ptr[GVariant], flags : GDBusCallFlags, timeout_msec : gint, fd_list : Ptr[GUnixFDList], cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_call_with_unix_fd_list_finish(proxy : Ptr[GDBusProxy], out_fd_list : Ptr[Ptr[GUnixFDList]], res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_call_with_unix_fd_list_sync(proxy : Ptr[GDBusProxy], method_name : Ptr[gchar], parameters : Ptr[GVariant], flags : GDBusCallFlags, timeout_msec : gint, fd_list : Ptr[GUnixFDList], out_fd_list : Ptr[Ptr[GUnixFDList]], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_cached_property(proxy : Ptr[GDBusProxy], property_name : Ptr[gchar]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_cached_property_names(proxy : Ptr[GDBusProxy]): Ptr[Ptr[gchar]] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_connection(proxy : Ptr[GDBusProxy]): Ptr[GDBusConnection] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_default_timeout(proxy : Ptr[GDBusProxy]): gint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_flags(proxy : Ptr[GDBusProxy]): GDBusProxyFlags = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_interface_info(proxy : Ptr[GDBusProxy]): Ptr[GDBusInterfaceInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_interface_name(proxy : Ptr[GDBusProxy]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_name(proxy : Ptr[GDBusProxy]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_name_owner(proxy : Ptr[GDBusProxy]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_object_path(proxy : Ptr[GDBusProxy]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_new(connection : Ptr[GDBusConnection], flags : GDBusProxyFlags, info : Ptr[GDBusInterfaceInfo], name : Ptr[gchar], object_path : Ptr[gchar], interface_name : Ptr[gchar], cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_new_finish(res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GDBusProxy] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_new_for_bus(bus_type : GBusType, flags : GDBusProxyFlags, info : Ptr[GDBusInterfaceInfo], name : Ptr[gchar], object_path : Ptr[gchar], interface_name : Ptr[gchar], cancellable : Ptr[GCancellable], callback : GAsyncReadyCallback, user_data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_new_for_bus_finish(res : Ptr[GAsyncResult], error : Ptr[Ptr[GError]]): Ptr[GDBusProxy] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_new_for_bus_sync(bus_type : GBusType, flags : GDBusProxyFlags, info : Ptr[GDBusInterfaceInfo], name : Ptr[gchar], object_path : Ptr[gchar], interface_name : Ptr[gchar], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GDBusProxy] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_new_sync(connection : Ptr[GDBusConnection], flags : GDBusProxyFlags, info : Ptr[GDBusInterfaceInfo], name : Ptr[gchar], object_path : Ptr[gchar], interface_name : Ptr[gchar], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GDBusProxy] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_set_cached_property(proxy : Ptr[GDBusProxy], property_name : Ptr[gchar], value : Ptr[GVariant]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_set_default_timeout(proxy : Ptr[GDBusProxy], timeout_msec : gint): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_proxy_set_interface_info(proxy : Ptr[GDBusProxy], info : Ptr[GDBusInterfaceInfo]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_send_message_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_server_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_server_get_client_address(server : Ptr[GDBusServer]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_server_get_flags(server : Ptr[GDBusServer]): GDBusServerFlags = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_server_get_guid(server : Ptr[GDBusServer]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_server_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_server_is_active(server : Ptr[GDBusServer]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_server_new_sync(address : Ptr[gchar], flags : GDBusServerFlags, guid : Ptr[gchar], observer : Ptr[GDBusAuthObserver], cancellable : Ptr[GCancellable], error : Ptr[Ptr[GError]]): Ptr[GDBusServer] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_server_start(server : Ptr[GDBusServer]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_server_stop(server : Ptr[GDBusServer]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_signal_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_signal_info_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_signal_info_ref(info : Ptr[GDBusSignalInfo]): Ptr[GDBusSignalInfo] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_signal_info_unref(info : Ptr[GDBusSignalInfo]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_subtree_flags_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_dbus_unescape_object_path(s : Ptr[gchar]): Ptr[guint8] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_add_toggle_ref(`object` : Ptr[GObject], notify : GToggleNotify, data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_add_weak_pointer(`object` : Ptr[GObject], weak_pointer_location : Ptr[gpointer]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_bind_property(source : gpointer, source_property : Ptr[gchar], target : gpointer, target_property : Ptr[gchar], flags : GBindingFlags): Ptr[GBinding] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_bind_property_full(source : gpointer, source_property : Ptr[gchar], target : gpointer, target_property : Ptr[gchar], flags : GBindingFlags, transform_to : GBindingTransformFunc, transform_from : GBindingTransformFunc, user_data : gpointer, notify : GDestroyNotify): Ptr[GBinding] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_bind_property_with_closures(source : gpointer, source_property : Ptr[gchar], target : gpointer, target_property : Ptr[gchar], flags : GBindingFlags, transform_to : Ptr[GClosure], transform_from : Ptr[GClosure]): Ptr[GBinding] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_class_find_property(oclass : Ptr[GObjectClass], property_name : Ptr[gchar]): Ptr[GParamSpec] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_class_install_properties(oclass : Ptr[GObjectClass], n_pspecs : guint, pspecs : Ptr[Ptr[GParamSpec]]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_class_install_property(oclass : Ptr[GObjectClass], property_id : guint, pspec : Ptr[GParamSpec]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_class_list_properties(oclass : Ptr[GObjectClass], n_properties : Ptr[guint]): Ptr[Ptr[GParamSpec]] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_class_override_property(oclass : Ptr[GObjectClass], property_id : guint, name : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_compat_control(what : gsize, data : gpointer): gsize = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_connect(`object` : gpointer, signal_spec : Ptr[gchar], rest: Any*): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_disconnect(`object` : gpointer, signal_spec : Ptr[gchar], rest: Any*): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_dup_data(`object` : Ptr[GObject], key : Ptr[gchar], dup_func : GDuplicateFunc, user_data : gpointer): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_dup_qdata(`object` : Ptr[GObject], quark : GQuark, dup_func : GDuplicateFunc, user_data : gpointer): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_force_floating(`object` : Ptr[GObject]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_freeze_notify(`object` : Ptr[GObject]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_get(`object` : gpointer, first_property_name : Ptr[gchar], rest: Any*): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_get_data(`object` : Ptr[GObject], key : Ptr[gchar]): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_get_property(`object` : Ptr[GObject], property_name : Ptr[gchar], value : Ptr[GValue]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_get_qdata(`object` : Ptr[GObject], quark : GQuark): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_get_valist(`object` : Ptr[GObject], first_property_name : Ptr[gchar], var_args : va_list): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_getv(`object` : Ptr[GObject], n_properties : guint, names : Ptr[Ptr[gchar]], values : Ptr[GValue]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_interface_find_property(g_iface : gpointer, property_name : Ptr[gchar]): Ptr[GParamSpec] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_interface_install_property(g_iface : gpointer, pspec : Ptr[GParamSpec]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_interface_list_properties(g_iface : gpointer, n_properties_p : Ptr[guint]): Ptr[Ptr[GParamSpec]] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_is_floating(`object` : gpointer): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_new(object_type : GType, first_property_name : Ptr[gchar], rest: Any*): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_new_valist(object_type : GType, first_property_name : Ptr[gchar], var_args : va_list): Ptr[GObject] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_new_with_properties(object_type : GType, n_properties : guint, names : Ptr[CString], values : Ptr[GValue]): Ptr[GObject] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_newv(object_type : GType, n_parameters : guint, parameters : Ptr[GParameter]): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_notify(`object` : Ptr[GObject], property_name : Ptr[gchar]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_notify_by_pspec(`object` : Ptr[GObject], pspec : Ptr[GParamSpec]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_ref(`object` : gpointer): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_ref_sink(`object` : gpointer): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_remove_toggle_ref(`object` : Ptr[GObject], notify : GToggleNotify, data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_remove_weak_pointer(`object` : Ptr[GObject], weak_pointer_location : Ptr[gpointer]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_replace_data(`object` : Ptr[GObject], key : Ptr[gchar], oldval : gpointer, newval : gpointer, destroy : GDestroyNotify, old_destroy : Ptr[GDestroyNotify]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_replace_qdata(`object` : Ptr[GObject], quark : GQuark, oldval : gpointer, newval : gpointer, destroy : GDestroyNotify, old_destroy : Ptr[GDestroyNotify]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_run_dispose(`object` : Ptr[GObject]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_set(`object` : gpointer, first_property_name : Ptr[gchar], rest: Any*): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_set_data(`object` : Ptr[GObject], key : Ptr[gchar], data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_set_data_full(`object` : Ptr[GObject], key : Ptr[gchar], data : gpointer, destroy : GDestroyNotify): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_set_property(`object` : Ptr[GObject], property_name : Ptr[gchar], value : Ptr[GValue]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_set_qdata(`object` : Ptr[GObject], quark : GQuark, data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_set_qdata_full(`object` : Ptr[GObject], quark : GQuark, data : gpointer, destroy : GDestroyNotify): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_set_valist(`object` : Ptr[GObject], first_property_name : Ptr[gchar], var_args : va_list): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_setv(`object` : Ptr[GObject], n_properties : guint, names : Ptr[Ptr[gchar]], values : Ptr[GValue]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_steal_data(`object` : Ptr[GObject], key : Ptr[gchar]): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_steal_qdata(`object` : Ptr[GObject], quark : GQuark): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_take_ref(`object` : gpointer): gpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_thaw_notify(`object` : Ptr[GObject]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_unref(`object` : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_watch_closure(`object` : Ptr[GObject], closure : Ptr[GClosure]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_weak_ref(`object` : Ptr[GObject], notify : GWeakNotify, data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_object_weak_unref(`object` : Ptr[GObject], notify : GWeakNotify, data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_add(builder : Ptr[GVariantBuilder], format_string : Ptr[gchar], rest: Any*): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_add_parsed(builder : Ptr[GVariantBuilder], format : Ptr[gchar], rest: Any*): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_add_value(builder : Ptr[GVariantBuilder], value : Ptr[GVariant]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_clear(builder : Ptr[GVariantBuilder]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_close(builder : Ptr[GVariantBuilder]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_end(builder : Ptr[GVariantBuilder]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_init(builder : Ptr[GVariantBuilder], `type` : Ptr[GVariantType]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_new(`type` : Ptr[GVariantType]): Ptr[GVariantBuilder] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_open(builder : Ptr[GVariantBuilder], `type` : Ptr[GVariantType]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_ref(builder : Ptr[GVariantBuilder]): Ptr[GVariantBuilder] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_builder_unref(builder : Ptr[GVariantBuilder]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_byteswap(value : Ptr[GVariant]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_check_format_string(value : Ptr[GVariant], format_string : Ptr[gchar], copy_only : gboolean): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_classify(value : Ptr[GVariant]): GVariantClass = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_compare(one : gconstpointer, two : gconstpointer): gint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_clear(dict : Ptr[GVariantDict]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_contains(dict : Ptr[GVariantDict], key : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_end(dict : Ptr[GVariantDict]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_get_type(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_init(dict : Ptr[GVariantDict], from_asv : Ptr[GVariant]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_insert(dict : Ptr[GVariantDict], key : Ptr[gchar], format_string : Ptr[gchar], rest: Any*): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_insert_value(dict : Ptr[GVariantDict], key : Ptr[gchar], value : Ptr[GVariant]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_lookup(dict : Ptr[GVariantDict], key : Ptr[gchar], format_string : Ptr[gchar], rest: Any*): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_lookup_value(dict : Ptr[GVariantDict], key : Ptr[gchar], expected_type : Ptr[GVariantType]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_new(from_asv : Ptr[GVariant]): Ptr[GVariantDict] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_ref(dict : Ptr[GVariantDict]): Ptr[GVariantDict] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_remove(dict : Ptr[GVariantDict], key : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dict_unref(dict : Ptr[GVariantDict]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dup_bytestring(value : Ptr[GVariant], length : Ptr[gsize]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dup_bytestring_array(value : Ptr[GVariant], length : Ptr[gsize]): Ptr[Ptr[gchar]] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dup_objv(value : Ptr[GVariant], length : Ptr[gsize]): Ptr[Ptr[gchar]] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dup_string(value : Ptr[GVariant], length : Ptr[gsize]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_dup_strv(value : Ptr[GVariant], length : Ptr[gsize]): Ptr[Ptr[gchar]] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_equal(one : gconstpointer, two : gconstpointer): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get(value : Ptr[GVariant], format_string : Ptr[gchar], rest: Any*): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_boolean(value : Ptr[GVariant]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_byte(value : Ptr[GVariant]): guint8 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_bytestring(value : Ptr[GVariant]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_bytestring_array(value : Ptr[GVariant], length : Ptr[gsize]): Ptr[Ptr[gchar]] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_child(value : Ptr[GVariant], `index_` : gsize, format_string : Ptr[gchar], rest: Any*): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_child_value(value : Ptr[GVariant], `index_` : gsize): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_data(value : Ptr[GVariant]): gconstpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_data_as_bytes(value : Ptr[GVariant]): Ptr[GBytes] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_double(value : Ptr[GVariant]): gdouble = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_fixed_array(value : Ptr[GVariant], n_elements : Ptr[gsize], element_size : gsize): gconstpointer = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_gtype(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_handle(value : Ptr[GVariant]): gint32 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_int16(value : Ptr[GVariant]): gint16 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_int32(value : Ptr[GVariant]): gint32 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_int64(value : Ptr[GVariant]): gint64 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_maybe(value : Ptr[GVariant]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_normal_form(value : Ptr[GVariant]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_objv(value : Ptr[GVariant], length : Ptr[gsize]): Ptr[Ptr[gchar]] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_size(value : Ptr[GVariant]): gsize = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_string(value : Ptr[GVariant], length : Ptr[gsize]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_strv(value : Ptr[GVariant], length : Ptr[gsize]): Ptr[Ptr[gchar]] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_type(value : Ptr[GVariant]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_type_string(value : Ptr[GVariant]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_uint16(value : Ptr[GVariant]): guint16 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_uint32(value : Ptr[GVariant]): guint32 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_uint64(value : Ptr[GVariant]): guint64 = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_va(value : Ptr[GVariant], format_string : Ptr[gchar], endptr : Ptr[Ptr[gchar]], app : Ptr[va_list]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_get_variant(value : Ptr[GVariant]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_hash(value : gconstpointer): guint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_is_container(value : Ptr[GVariant]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_is_floating(value : Ptr[GVariant]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_is_normal_form(value : Ptr[GVariant]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_is_object_path(string : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_is_of_type(value : Ptr[GVariant], `type` : Ptr[GVariantType]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_is_signature(string : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_iter_copy(iter : Ptr[GVariantIter]): Ptr[GVariantIter] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_iter_free(iter : Ptr[GVariantIter]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_iter_init(iter : Ptr[GVariantIter], value : Ptr[GVariant]): gsize = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_iter_loop(iter : Ptr[GVariantIter], format_string : Ptr[gchar], rest: Any*): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_iter_n_children(iter : Ptr[GVariantIter]): gsize = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_iter_new(value : Ptr[GVariant]): Ptr[GVariantIter] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_iter_next(iter : Ptr[GVariantIter], format_string : Ptr[gchar], rest: Any*): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_iter_next_value(iter : Ptr[GVariantIter]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_lookup(dictionary : Ptr[GVariant], key : Ptr[gchar], format_string : Ptr[gchar], rest: Any*): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_lookup_value(dictionary : Ptr[GVariant], key : Ptr[gchar], expected_type : Ptr[GVariantType]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_n_children(value : Ptr[GVariant]): gsize = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new(format_string : Ptr[gchar], rest: Any*): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_array(child_type : Ptr[GVariantType], children : Ptr[Ptr[GVariant]], n_children : gsize): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_boolean(value : gboolean): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_byte(value : guint8): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_bytestring(string : Ptr[gchar]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_bytestring_array(strv : Ptr[Ptr[gchar]], length : gssize): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_dict_entry(key : Ptr[GVariant], value : Ptr[GVariant]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_double(value : gdouble): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_fixed_array(element_type : Ptr[GVariantType], elements : gconstpointer, n_elements : gsize, element_size : gsize): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_from_bytes(`type` : Ptr[GVariantType], bytes : Ptr[GBytes], trusted : gboolean): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_from_data(`type` : Ptr[GVariantType], data : gconstpointer, size : gsize, trusted : gboolean, notify : GDestroyNotify, user_data : gpointer): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_handle(value : gint32): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_int16(value : gint16): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_int32(value : gint32): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_int64(value : gint64): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_maybe(child_type : Ptr[GVariantType], child : Ptr[GVariant]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_object_path(object_path : Ptr[gchar]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_objv(strv : Ptr[Ptr[gchar]], length : gssize): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_parsed(format : Ptr[gchar], rest: Any*): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_parsed_va(format : Ptr[gchar], app : Ptr[va_list]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_printf(format_string : Ptr[gchar], rest: Any*): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_signature(signature : Ptr[gchar]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_string(string : Ptr[gchar]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_strv(strv : Ptr[Ptr[gchar]], length : gssize): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_take_string(string : Ptr[gchar]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_tuple(children : Ptr[Ptr[GVariant]], n_children : gsize): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_uint16(value : guint16): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_uint32(value : guint32): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_uint64(value : guint64): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_va(format_string : Ptr[gchar], endptr : Ptr[Ptr[gchar]], app : Ptr[va_list]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_new_variant(value : Ptr[GVariant]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_parse(`type` : Ptr[GVariantType], text : Ptr[gchar], limit : Ptr[gchar], endptr : Ptr[Ptr[gchar]], error : Ptr[Ptr[GError]]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_parse_error_print_context(error : Ptr[GError], source_str : Ptr[gchar]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_parse_error_quark(): GQuark = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_parser_get_error_quark(): GQuark = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_print(value : Ptr[GVariant], type_annotate : gboolean): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_print_string(value : Ptr[GVariant], string : Ptr[GString], type_annotate : gboolean): Ptr[GString] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_ref(value : Ptr[GVariant]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_ref_sink(value : Ptr[GVariant]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_store(value : Ptr[GVariant], data : gpointer): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_take_ref(value : Ptr[GVariant]): Ptr[GVariant] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_checked_(type_string : Ptr[gchar]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_copy(`type` : Ptr[GVariantType]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_dup_string(`type` : Ptr[GVariantType]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_element(`type` : Ptr[GVariantType]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_equal(type1 : gconstpointer, type2 : gconstpointer): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_first(`type` : Ptr[GVariantType]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_free(`type` : Ptr[GVariantType]): Unit = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_get_gtype(): GType = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_get_string_length(`type` : Ptr[GVariantType]): gsize = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_hash(`type` : gconstpointer): guint = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_is_array(`type` : Ptr[GVariantType]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_is_basic(`type` : Ptr[GVariantType]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_is_container(`type` : Ptr[GVariantType]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_is_definite(`type` : Ptr[GVariantType]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_is_dict_entry(`type` : Ptr[GVariantType]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_is_maybe(`type` : Ptr[GVariantType]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_is_subtype_of(`type` : Ptr[GVariantType], supertype : Ptr[GVariantType]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_is_tuple(`type` : Ptr[GVariantType]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_is_variant(`type` : Ptr[GVariantType]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_key(`type` : Ptr[GVariantType]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_n_items(`type` : Ptr[GVariantType]): gsize = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_new(type_string : Ptr[gchar]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_new_array(element : Ptr[GVariantType]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_new_dict_entry(key : Ptr[GVariantType], value : Ptr[GVariantType]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_new_maybe(element : Ptr[GVariantType]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_new_tuple(items : Ptr[Ptr[GVariantType]], length : gint): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_next(`type` : Ptr[GVariantType]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_peek_string(`type` : Ptr[GVariantType]): Ptr[gchar] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_string_get_depth_(type_string : Ptr[gchar]): gsize = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_string_is_valid(type_string : Ptr[gchar]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_string_scan(string : Ptr[gchar], limit : Ptr[gchar], endptr : Ptr[Ptr[gchar]]): gboolean = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_type_value(`type` : Ptr[GVariantType]): Ptr[GVariantType] = extern

/**
 * [bindgen] header: /tmp/nix-shell.x3r5aH/scala-tools-libgio-P5J4T3SMTpj.h
*/
@extern def g_variant_unref(value : Ptr[GVariant]): Unit = extern