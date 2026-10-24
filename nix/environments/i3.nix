{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.i3;
  multi-i3status = pkgs.rustPlatform.buildRustPackage rec {
    name = "multi-i3status";
    src = pkgs.fetchFromGitHub {
      owner = "Endered";
      repo = name;
      rev = "09c4dfd981b6cf1cdb3e67c186b349b1e836c20d";
      sha256 = "sha256-BkLsVB+zeauWkojeQsX9UuSs7JPS2xAAF5bc++kUCuE=";
    };
    cargoHash = "sha256-/BJvq/AEMMxOfN+hTpQovm3GjZwniKzxKrnofRAyc7k=";
  };
  i3statusviewer = pkgs.stdenv.mkDerivation {
    pname = "i3statusviewer";
    version = "0.0.2";

    src = pkgs.fetchurl {
      url = "https://github.com/Endered/i3statusviewer/releases/download/v0.0.2/i3statusviewer";
      hash = "sha256-3JcNpXmkfmoA5E2MB8WcN3CzcMDbkybE4FG8X/PNaOY=";
    };

    dontBuild = true;
    dontUnpack = true;

    installPhase = ''
        install -Dm 755 $src $out/bin/i3statusviewer
        ln -s $out/bin/i3statusviewer $out/bin/i3statusviewer-viewer
        ln -s $out/bin/i3statusviewer $out/bin/i3statusviewer-translator
      '';
  };
in
{
  options.my-settings.i3 = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
    extraOpts = lib.mkOption {
      default = "";
      type = lib.types.lines;
    };
  };

  config = lib.mkIf (!cfg.disable)
    {
      home.packages = with pkgs; [ i3status-rust multi-i3status i3statusviewer ];
      home.file = {
        ".config/i3status-rust/config.toml" = {
          source = ~/dotfiles/config/i3status-rust/config.toml;
        };
        ".config/i3/config" = {
          text = ''
          ${builtins.readFile ~/dotfiles/config/i3/config}
          ${cfg.extraOpts}
          '';
        };
        ".config/i3status/config" = {
          source = ~/dotfiles/config/i3status/config;
        };
      };
    };
}
