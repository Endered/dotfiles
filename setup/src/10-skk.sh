#!/usr/bin/env bash

SKK_DICT_DIR=$HOME/.local/share/work/skk/
SKK_DICT_CONF_DIR=$HOME/.local/share/fcitx5/skk/
FCITX_CONF_DIR=$HOME/.config/fcitx5/conf/

mkdir -p $SKK_DICT_DIR
mkdir -p $SKK_DICT_CONF_DIR
mkdir -p $FCITX_CONF_DIR

tmpdir=$(mktemp -d)
pushd $tmpdir

git clone https://github.com/Endered/skk-emoticon-from-mozc
pushd skk-emoticon-from-mozc
bash main.sh
cp skk-emoticon.utf8 $SKK_DICT_DIR
popd

curl https://raw.githubusercontent.com/tokuhirom/jawiki-kana-kanji-dict/0e0fb52af809d6f675bf1ba5a828812715957762/SKK-JISYO.jawiki -o $SKK_DICT_DIR/SKK-JISYO.jawiki
curl https://raw.githubusercontent.com/skk-dev/dict/master/SKK-JISYO.L -o $SKK_DICT_DIR/SKK-JISYO.L

SKK_DICT_CONF_DIR=$HOME/.local/share/fcitx5/skk/
cat <<EOF > $SKK_DICT_CONF_DIR/dictionary_list
file=$HOME/.local/share/work/skk/SKK-JISYO.L,mode=readonly,type=file
encoding=UTF-8,file=$HOME/.local/share/work/skk/SKK-JISYO.jawiki,mode=readonly,type=file
encoding=UTF-8,file=$HOME/.local/share/work/skk/skk-emoticon.utf8,mode=readonly,type=file
EOF


cp $HOME/dotfiles/config/fcitx5/conf//skk.conf $HOME/.config/fcitx5/conf/skk.conf
cp -r $HOME/dotfiles/config/libskk $HOME/libskk

popd
rm -rf $tmpdir
