{ pkgs, lib, ... }:
let toumi = pkgs.stdenv.mkDerivation {
      pname = "toumi";
      version = "0.0.5";

      src = pkgs.fetchurl {
        url = "https://github.com/Endered/toumi/releases/download/v0.0.5/toumi";
        hash = "sha256-TNxj/rf97BPqjA74fnuKWy/33WNLkn6APL2B1e4Pu6w=";
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
    
