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
      scala-tools = sbt.mkSbtDerivation.x86_64-linux {
        pname = "scala-tools";
        version = "0.0.1";

        buildInputs = with pkgs; [
          clang
          boehmgc
        ];

        nativeBuildInputs = with pkgs; [
          which
        ];

        src = pkgs.nix-gitignore.gitignoreSource [] ./.;

        depsSha256 = "sha256-WmHxOBuyuCZKIjJaidlWLsGOOd/mC48gKm0xJfEC3Uc=";

        buildPhase = ''
      sbt nativeLinkReleaseFull
      '';

        installPhase = ''
        install -D -m 0755 target/scala-3.8.3/root-release-full $out/bin/scala-tools
      '';

        dontFixup = true;
      };
    in {
      packages.x86_64-linux.default = pkgs.runCommand "wrapped-scala-tools" {} ''
        mkdir -p $out/bin
        ln -s ${scala-tools}/bin/scala-tools $out/bin/my-cpu-measure-tool
        ln -s ${scala-tools}/bin/scala-tools $out/bin/sound-changer
      '';
    };
}
