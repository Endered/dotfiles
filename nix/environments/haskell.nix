{ pkgs, ... }:
{
  home.packages = [
    (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      stack
      haskell-language-server
    ]))
  ];
}
