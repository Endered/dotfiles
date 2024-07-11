{ pkgs ? import <nixpkgs> {}}:
let
  monapo-zip = pkgs.fetchurl {
    url= "https://fonts.aahub.org/assets/fonts/monapo/monapo.zip";
    hash = "sha256-6H8gnLjCRDPGN94W7O4P81ozYEXbohsjTR6kKNii3o0=";
  };
in
pkgs.stdenv.mkDerivation {
  pname = "monapo-font";
  phases = [ "buildPhase" "installPhase" ];

  nativeBuildInputs = with pkgs; [
    unzip
  ];

  buildPhase = ''
  unzip ${monapo-zip}
  '';

  installPhase = ''
  install -Dm 644 ./monapo/monapo.ttf $out/share/fonts/truetype/monapo.ttf
  '';
}
