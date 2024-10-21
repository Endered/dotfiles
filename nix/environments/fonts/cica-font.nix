{ pkgs ? import <nixpkgs> {}}:
let
  monapo-zip = pkgs.fetchurl {
    url= "https://github.com/miiton/Cica/releases/download/v5.0.3/Cica_v5.0.3_without_emoji.zip";
    hash = "sha256-4UyVyMPpjTd4Yy9Xeiyvpp7oLNjC79sfOMgh43xIfb8=";
  };
in
pkgs.stdenv.mkDerivation {
  name = "cica-font";
  phases = [ "buildPhase" "installPhase" ];

  nativeBuildInputs = with pkgs; [
    unzip
  ];

  buildPhase = ''
  unzip ${monapo-zip}
  '';

  installPhase = ''
  install -d $out/share/fonts/truetype
  install -Dm 644 ./*.ttf $out/share/fonts/truetype
  '';
}
