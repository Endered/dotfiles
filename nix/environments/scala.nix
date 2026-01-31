{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.scala;
  unstable-pkgs = import <nixpkgs-unstable> {};
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
      unstable-pkgs.sbt
      (pkgs.writeShellScriptBin "metals"
        ''exec ${pkgs.emacs-lsp-booster}/bin/emacs-lsp-booster --disable-bytecode -- ${unstable-pkgs.metals}/bin/metals "$@"'')
      # It use a hacky way for inject clang to scala-cli without global install
      (writeScriptBin "scala-cli" ''
      #!/usr/bin/env bash
      export PATH=${clang}/bin:$PATH
      exec ${scala-cli}/bin/scala-cli "$@"
      '')
    ];
  };
}
