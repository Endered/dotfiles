{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.scala;
in
{
  options.my-settings.scala = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = with pkgs; [
      sbt metals
    ];
  };
}
