{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.scala;
  my-metals = pkgs.stdenv.mkDerivation (finalAttrs: {
    # copied from https://github.com/NixOS/nixpkgs/issues/485281
    name = "metals";
    version = "2.0.0-M16";

    deps = pkgs.stdenv.mkDerivation {
      name = "metals-deps";
      version = finalAttrs.version;
      buildCommand = ''
        export COURSIER_CACHE=$(pwd)
        mkdir -p $out/bin
        ${pkgs.coursier}/bin/cs bootstrap org.scalameta:metals_2.13:${finalAttrs.version} \
          -r bintray:scalacenter/releases \
          -r sonatype:snapshots \
          --repository "https://central.sonatype.com/repository/maven-snapshots" \
          --standalone \
          -o $out/bin/metals-launcher
      '';
      outputHashMode = "recursive";
      outputHashAlgo = "sha256";
      outputHash = "sha256-AOcUfkJFKDgH8IMZQo1W6P+OZkgzo2xS6bdLp8WMrTQ=";
    };

    nativeBuildInputs = [ pkgs.makeWrapper ];
    buildInputs = [ finalAttrs.deps ];
    dontUnpack = true;
    extraJavaOpts = 
      "-XX:+UseG1GC" +
      "-XX:+UseStringDeduplication" +
      "-Xss4m" +
      "-Xms100m";

    installPhase = ''
      mkdir -p $out/bin

      makeWrapper ${finalAttrs.deps}/bin/metals-launcher $out/bin/metals \
        --set JAVA_HOME ${pkgs.jre} --add-flags ${finalAttrs.extraJavaOpts}
    '';
  });
  my-sbt = pkgs.sbt.overrideAttrs (prev : rec {
    version = "2.0.1";
    src = prev.src.overrideAttrs (_: {
      url = "https://github.com/sbt/sbt/releases/download/v${version}/sbt-${version}.tgz";
      hash = "sha256-dQ7GGY12eaTBgQuNTWNJGK0SGw18ZN9onAur3rhF17g=";
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
      my-sbt
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
