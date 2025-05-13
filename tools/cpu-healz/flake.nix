{
  description = "A tool for testing Flakes and Scala Native";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    sbt.url = "github:zaninime/sbt-derivation";
    sbt.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, sbt }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
    packages.x86_64-linux.default = sbt.mkSbtDerivation.x86_64-linux {
      pname = "my-cpu-measure-tool";
      version = "0.0.1";

      buildInputs = with pkgs; [
        clang
        boehmgc
      ];

      nativeBuildInputs = with pkgs; [
        which
      ];

      src = ./.;

      depsSha256 = "sha256-vx7NSKWiGgPMsOniJRZ9yjgj9JtpiZk5QZzU/qvAUmU=";

      buildPhase = ''
        sbt nativeLinkReleaseFull
      '';

      installPhase = ''
        install -D -m 0755 target/scala-3.6.3/cpu-healz-release-full $out/bin/my-cpu-measure-tool
      '';

      dontFixup = true;
    };
  };
}
