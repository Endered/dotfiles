{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.scala;
  my-metals = pkgs.metals.overrideAttrs (prev: {
    version = "1.6.7";
    deps = prev.deps.overrideAttrs (_: {
      outputHash = "sha256-bGx3PQGgaTueQ/v/Xk7gp03TzllyMs7nCx9QWXNFdt0=";
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
