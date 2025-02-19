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
    cargoHash = "sha256-WPfeXDA63KVzyW/V4xSnWlup4T9x/DFQ8dfzy7NI9hM=";
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
      home.packages = with pkgs; [ i3status-rust multi-i3status ];
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
