{ pkgs, ... }:
{
  home.packages = with pkgs; [
    gcc
    gnumake
    cmake
    glib
    clang-tools
  ];
}
