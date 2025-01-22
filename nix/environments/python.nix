{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.python;
in
{
  options.my-settings.python = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = with pkgs; [
      (python3.withPackages (p: with p; [
        epc
        orjson
        packaging
        sexpdata
        six
        setuptools
        paramiko
        rapidfuzz
        watchdog
      ]))
    ];
  };
}
