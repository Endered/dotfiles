{ pkgs, lib, ... }:
let toumi = pkgs.stdenv.mkDerivation {
      pname = "toumi";
      version = "0.0.1";

      src = pkgs.fetchurl {
        url = "https://github.com/Endered/toumi/releases/download/v0.0.1/toumi";
        hash = "sha256-aJ3mvPlcNCUGOEjyzl+7s1wzmwoSfizKJa7AKTscnAQ=";
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
    
