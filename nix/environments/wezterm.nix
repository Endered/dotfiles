{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.wezterm;
  appimage-version = pkgs.fetchurl {
    url = "https://github.com/wez/wezterm/releases/download/20240203-110809-5046fc22/WezTerm-20240203-110809-5046fc22-Ubuntu20.04.AppImage";
    hash = "sha256-NAEKBwdtInLE1PlLXg2uYIpnlZno1ylEYyP4j5VsYPA=";
  };
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
          source = ~/dotfiles/wezterm.lua;
        };
      };
    };
}
