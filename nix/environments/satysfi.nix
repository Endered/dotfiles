{ lib, pkgs, ... }:
let
  satysfi-language-server-source = pkgs.fetchFromGitHub {
      owner = "monaqa";
      repo = "satysfi-language-server";
      rev = "1ce6bc4d08eb748aeb10f69498e4a16f01978535";
      sha256 = "sha256-4EmLDsCrJXzQb72JrGGPR7+gAQKcniVGrBnrl9JanBs=";
    };
  satysfi-language-server = pkgs.rustPlatform.buildRustPackage rec {
    name = "satysfi-language-server";
    src = satysfi-language-server-source;
    hash = "";
    cargoLock =  {
      lockFile = ./satysfi/Cargo.lock;
      outputHashes = {
        "satysfi-formatter-0.1.0" = "sha256-a2meR+4OnJkXzr4vqbTToZ9s2aXarKIRtqLUyALYVyQ=";
        "satysfi-parser-0.0.3" = "sha256-JzC6iRAvbRU5wovZzWwq1q535/WrOamucnbcLIZn/kg=";
      };
    };
    cargoPatches = [
      ./satysfi/cargo.patch
    ];
  };
in
{
  home.packages = [
    satysfi-language-server
    satysfi
  ];

  home.file = {
    ".emacs.d/lisp/satysfi.el" = {
      source = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/gfngfn/satysfi.el/master/satysfi.el";
        hash = "sha256-sIbxA0V06YvakigcoB0GocOtzUqliNbeZlV6REhZwkg=";
      };
    };
  };
}
