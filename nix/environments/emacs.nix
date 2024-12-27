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

    home.file = {
      ".emacs.d/lisp/eglot-booster.el" = {
        source = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/jdtsmith/eglot-booster/refs/heads/main/eglot-booster.el";
          hash = "sha256-MGMKFtivqM979xf2H0XinLvlVhp23Ynd1+UqTxD3eZY=";
        };
      };
    };
  };
}
