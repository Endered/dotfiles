{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.cpp;
in
{
  options.my-settings.cpp = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = with pkgs; [
      gcc
      gnumake
      cmake
      glib
      clang-tools
    ];
  };
}
