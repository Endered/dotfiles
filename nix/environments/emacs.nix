{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.emacs;
in
{
  options.my-settings.emacs = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = with pkgs; [
      emacs-lsp-booster
    ];

    programs.emacs = {
      enable = true;
      package = pkgs.emacs29;
      extraPackages = epkgs: [
        epkgs.vterm
        epkgs.pdf-tools
      ];
    };
  };
}
