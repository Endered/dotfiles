{pkgs, ...}:
let
  skk-emoticon-from-mozc = import (pkgs.fetchFromGitHub {
    owner = "Endered";
    repo = "skk-emoticon-from-mozc";
    rev = "b3345dea1295119ac0b97bc8a4029628f2693d93";
    sha256 = "sha256-l0IZYztvGhevh+tH+kEBjLXyE4wOWptoblwhCS1MdIY=";
  }) {};
  home = builtins.getEnv "HOME";
in
{
  home.file = {
    ".local/share/work/skk/skk-emoticon.utf8" = {
      source = skk-emoticon-from-mozc + "/share/skk-emoticon.utf8";
    };
    ".local/share/work/skk/SKK-JISYO.jawiki" = {
      source = pkgs.fetchurl {
        url = "https://github.com/tokuhirom/jawiki-kana-kanji-dict/raw/0e0fb52af809d6f675bf1ba5a828812715957762/SKK-JISYO.jawiki";
        hash = "sha256-NxK6mDmDizoAstNAV4PcBZ5sZ84DM3TmT3iJGPQApFE=";
      };
    };
    ".local/share/work/skk/SKK-JISYO.L" = {
      source = pkgs.skk-dicts + "/share/SKK-JISYO.L";
    };
    ".local/share/fcitx5/skk/dictionary_list" = {
      text = ''
        file=${home}/.local/share/work/skk/SKK-JISYO.L,mode=readonly,type=file
        encoding=UTF-8,file=${home}/.local/share/work/skk/SKK-JISYO.jawiki,mode=readonly,type=file
        encoding=UTF-8,file=${home}/.local/share/work/skk/skk-emoticon.utf8,mode=readonly,type=file
      '';
    };
    ".config/fcitx5/conf/skk.conf" = {
      source = ~/dotfiles/config/fcitx5/conf/skk.conf;
    };
  };
}
