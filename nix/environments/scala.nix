{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.scala;
  my-metals = pkgs.metals.overrideAttrs (prev: {
    version = "2.0.0-M14";
    deps = prev.deps.overrideAttrs (_: {
      outputHash = "sha256-mPw1K2RQfebX3FYy/e3zeBaLNr+3REBmbFrjeG+74YM=";
    });
  });
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
      sbt
      (pkgs.writeShellScriptBin "metals"
        ''exec ${pkgs.emacs-lsp-booster}/bin/emacs-lsp-booster --disable-bytecode -- ${my-metals}/bin/metals "$@"'')
      # It use a hacky way for inject clang to scala-cli without global install
      (writeScriptBin "scala-cli" ''
      #!/usr/bin/env bash
      export PATH=${clang}/bin:$PATH
      exec ${scala-cli}/bin/scala-cli "$@"
      '')
    ];
  };
}
