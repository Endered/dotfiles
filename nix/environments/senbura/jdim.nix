{ pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation rec {
  version = "0.12.0";
  pname = "JDim";

  src = pkgs.fetchFromGitHub {
    owner = "JDimproved";
    repo = pname;
    rev = "JDim-v0.16.0";
    hash = "sha256-XflYZyukPANNSLsmP9ZWVpVdYiMzTyHvvdso9ay2VBQ=";
  };

  buildInputs = with pkgs; [
    meson
    ninja
    gnutls
    xorg.libSM
    gtkmm3
    cmake
    libxcrypt
    gtest
  ];

  nativeBuildInputs = with pkgs; [
    pkg-config
  ];
}
