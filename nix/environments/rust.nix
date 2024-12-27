{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.rust;
in
{
  options.my-settings.rust = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = with pkgs; [
      rustc
      rustfmt
      cargo
      rust-analyzer
    ];
  };
}
