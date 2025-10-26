{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.sway;
in
{
  options.my-settings.sway = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable)
    {
      home.file = {
        ".config/sway/config" = {
          source = ~/dotfiles/config/sway/config;
        };
      };
    };
}
