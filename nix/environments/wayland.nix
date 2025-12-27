{ config, lib, pkgs, ... }:
let 
  cfg = config.my-settings.i3;
in
{
  options.my-settings.wayland = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable)
    {
      home.packages = with pkgs; [
        wl-clipboard
        wdisplays
      ];
    };
}
