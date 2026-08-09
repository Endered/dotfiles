{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.emacs;
  lsp-bridge = pkgs.fetchFromGitHub {
    owner = "manateelazycat";
    repo = "lsp-bridge";
    rev = "6fd5eb21a174e6a04247a2f370b544dcd6cb2420";
    sha256 = "sha256-+E1l0Ea0Db5ksX9tDW+cvNUMjT4be5i9qcI/rIvFKbY=";
  };
  patch-grammars = grammars: grammars // {
    tree-sitter-scala = (pkgs.lib.makeOverridable pkgs.tree-sitter.buildGrammar) {
      language = "scala";
      version = "0.26.2";
      src = pkgs.fetchFromGitHub {
        owner = "tree-sitter";
        repo = "tree-sitter-scala";
        tag = "v0.26.2";
        hash = "sha256-PRyNcsiGeGfKtHvbLaGtiog/P8QEs117rqoBZZOXbeE=";
      };
    };
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
      package = pkgs.emacs30;
      extraPackages = epkgs: [
        epkgs.vterm
        epkgs.pdf-tools
        (epkgs.treesit-grammars.with-grammars (p: builtins.attrValues (patch-grammars p)))
      ];
    };

    home.file = {
      ".emacs.d/lisp/lsp-bridge" = {
        source = lsp-bridge;
      };
    };
  };
}
