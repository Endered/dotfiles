{ pkgs, lib, ... }:
let toumi = pkgs.stdenv.mkDerivation {
      pname = "toumi";
      version = "0.0.3";

      src = pkgs.fetchurl {
        url = "https://github.com/Endered/toumi/releases/download/v0.0.3/toumi";
        hash = "sha256-6bsfGKC9PvFvmmQ/4Ta49BSPoPQKUGakMgU3tdget4A=";
      };

      dontBuild = true;
      dontUnpack = true;

      installPhase = ''
        install -Dm 755 $src $out/bin/toumi
      '';
    };
in {
  home.packages = [
    toumi
  ];
}
    
