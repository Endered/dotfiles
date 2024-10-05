{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.lua;
in
{
  options.my-settings.lua = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = with pkgs; [
      luajit
      luajitPackages.luarocks
    ];
  };
}
