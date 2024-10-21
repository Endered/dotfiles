{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.fonts;
  monapo = import ./fonts/monapo-font.nix {};
  cica = import ./fonts/cica-font.nix {};
in
{
  options.my-settings.fonts = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = with pkgs; [
      monapo
      cica
    ];

    home.file = {
      ".config/fontconfig/fonts.conf" = {
        source = ~/dotfiles/config/fontconfig/fonts.conf;
      };
    };
  };
}
