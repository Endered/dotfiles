{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.haskell;
in
{

  options.my-settings.haskell = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config =  lib.mkIf (!cfg.disable) {
    home.packages = [
      (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
        stack
        haskell-language-server
      ]))
    ];
  };
}
