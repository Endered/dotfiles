{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.latex;
in
{
  options.my-settings.latex = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = with pkgs; [
      texlive.combined.scheme-full
      texlab
    ];

    home.file = {
      ".config/latexmk/latexmkrc" = {
        source = ~/dotfiles/config/latexmk/latexmkrc;
      };
    };
  };
}
