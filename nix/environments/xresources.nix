{ config, lib, ... }:
let
  cfg = config.my-settings.xresources;
in
{
  options.my-settings.xresources = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };

    dpi = lib.mkOption {
      default = 120;
      type = lib.types.number;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    xresources.properties = {
      "Xft.dpi" = cfg.dpi;
    };
  };
}
