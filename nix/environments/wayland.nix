{ config, lib, pkgs, ... }:
let 
  cfg = config.my-settings.i3;
  screenshot = pkgs.writeShellScriptBin "screenshot" ''
    exec ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)"
  '';
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
        grim
        slurp
        screenshot
      ];

      services.fnott = {
        enable = true;
        configFile = ~/dotfiles/config/fnott/fnott.ini;
      };
    };
}
