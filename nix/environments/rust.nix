{ pkgs, ... }:
{
  home.packages = with pkgs; [
    rustc
    rustfmt
    cargo
    rust-script
    rust-analyzer
  ];
}
