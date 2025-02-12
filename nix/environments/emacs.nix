{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.emacs;
  lsp-bridge = pkgs.fetchFromGitHub {
    owner = "manateelazycat";
    repo = "lsp-bridge";
    rev = "6fd5eb21a174e6a04247a2f370b544dcd6cb2420";
    sha256 = "sha256-+E1l0Ea0Db5ksX9tDW+cvNUMjT4be5i9qcI/rIvFKbY=";
  };
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
        epkgs.treesit-grammars.with-all-grammars
      ];
    };

    home.file = {
      ".emacs.d/lisp/lsp-bridge" = {
        source = lsp-bridge;
      };
    };
  };
}
