#!/usr/bin/env bash

tmp_file=$(mktemp scala-tools-libgio-XXXXXXXXXXX.h --tmpdir)

toumi $(pkg-config --cflags gio-2.0) \
      -t 'gio/gio.h' \
      -p 'g_variant.*' \
      -p 'g_dbus.*' \
      -p 'g_object.*' \
      -p 'gchar' -p 'gconstpointer' -p 'gint' -p 'gsize' \
    | cat <(echo '#include<gio/gio.h>') \
	  <(echo '#undef g_object_ref') \
	  <(echo '#undef g_object_ref_sink') \
	  -  \
	  > $tmp_file

mkdir -p libgio/src/main/scala/generated
mkdir -p libgio/src/main/resources/scala-native/generated

sn-bindgen --package libgio \
	   --header $tmp_file \
	   --multi-file --out libgio/src/main/scala/generated \
	   --scala \
	   -- $(pkg-config --cflags gio-2.0)


sn-bindgen --package libgio \
	   --header $tmp_file \
	   --out libgio/src/main/resources/scala-native/generated \
	   --c \
	   -- $(pkg-config --cflags gio-2.0)


rm $tmp_file
