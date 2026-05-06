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
      my-sn-bindgen = pkgs.stdenv.mkDerivation {
        pname = "sn-bindgen";
        version = "0.4.4";
        src = pkgs.fetchurl {
          url = "https://github.com/indoorvivants/sn-bindgen/releases/download/v0.4.4/sn-bindgen-x86_64-pc-linux";
          sha256 = "sha256-z05m0cwM9ubrOiMKXZUKQqR9f9drtEY0owN+4dL+sqM=";
        };

        dontUnpack = true;

        buildInputs = [
          pkgs.llvmPackages_17.libclang
        ];

        nativeBuildInpust = [
        ];

        installPhase = ''
          install -D -m 0755 $src $out/bin/sn-bindgen
          patchelf --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) $out/bin/sn-bindgen
          patchelf --replace-needed libclang-17.so.17 libclang.so.17 $out/bin/sn-bindgen
          patchelf --set-rpath ${pkgs.lib.makeLibraryPath [pkgs.llvmPackages_17.libclang]} $out/bin/sn-bindgen
        '';
        debug = true;
      };
      scala-tools = sbt.mkSbtDerivation.x86_64-linux {
        pname = "scala-tools";
        version = "0.0.1";

        buildInputs = with pkgs; [
          boehmgc
          glib
        ];

        nativeBuildInputs = with pkgs; [
          which
          clang_17
          pkg-config
          my-sn-bindgen

          glib # required for sbt-derivation's depsWarmup
        ];

        src = pkgs.nix-gitignore.gitignoreSource [] ./.;

        depsSha256 = "sha256-ldPHOLaeI1OlWg5gqlaRek6D2xXs2wVg9Tyk3GTMJxA=";

        depsWarmupCommand = ''
          export BINDGEN_PATH="${my-sn-bindgen}/bin/sn-bindgen"
	  pkg-config --cflags gio-2.0
          sbt -mem 4096 'compile'
        '';

        buildPhase = ''
          sbt -mem 4096 nativeLinkReleaseFull
        '';

        installPhase = ''
          install -D -m 0755 target/scala-3.8.3/root-release-full $out/bin/scala-tools
        '';

        BINDGEN_PATH = "${my-sn-bindgen}/bin/sn-bindgen";

        dontFixup = true;
      };
    in {
      packages.x86_64-linux.scala-tools = scala-tools;
      packages.x86_64-linux.default = pkgs.runCommand "wrapped-scala-tools" {} ''
        mkdir -p $out/bin
        ln -s ${scala-tools}/bin/scala-tools $out/bin/my-cpu-measure-tool
        ln -s ${scala-tools}/bin/scala-tools $out/bin/sound-changer
        ln -s ${scala-tools}/bin/scala-tools $out/bin/bluetooth-battery-watcher
      '';
    };
}
