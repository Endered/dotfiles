{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.senbura;
  jdim = import ./senbura/jdim.nix {};
in
{
  options.my-settings.senbura = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = with pkgs; [
      jdim
    ];
  };
}
