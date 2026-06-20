{ pkgs, lib, ... }:
let toumi = pkgs.stdenv.mkDerivation {
      pname = "toumi";
      version = "0.0.2";

      src = pkgs.fetchurl {
        url = "https://github.com/Endered/toumi/releases/download/v0.0.2/toumi";
        hash = "sha256-M6TdVN/OayuMkLwvlsY7PvUaQSvuJZOWWb0mXZ0LOFo=";
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
    
