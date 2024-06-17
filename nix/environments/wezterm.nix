{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.wezterm;
  appimage-version = pkgs.fetchurl {
    url = "https://github.com/wez/wezterm/releases/download/20240203-110809-5046fc22/WezTerm-20240203-110809-5046fc22-Ubuntu20.04.AppImage";
    hash = "sha256-NAEKBwdtInLE1PlLXg2uYIpnlZno1ylEYyP4j5VsYPA=";
  };
  pure-setting-file = ~/dotfiles/wezterm.lua;
  font-size = toString cfg.font-size;
  setting-file = pkgs.runCommand "modyfied-wezterm.lua" {} ''
               cat ${pure-setting-file} |
                 sed -e 's/font_size.*/font_size = ${font-size},/' > $out
  '';
in
{
  options.my-settings.wezterm = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
    use-appimage = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
    font-size = lib.mkOption {
      default = 9;
      type = lib.types.number;
    };
  };

  config = lib.mkIf (!cfg.disable)
    {
      home.packages = lib.mkIf (!cfg.use-appimage) [ pkgs.wezterm ];
      home.file = {
        "bin/wezterm" = {
          enable = cfg.use-appimage;
          source = appimage-version;
          executable = true;
        };
        ".wezterm.lua" = {
          source = setting-file;
        };
      };
    };
}
