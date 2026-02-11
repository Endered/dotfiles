{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.file-open;
in
{
  options.my-settings.file-open = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = [
      (pkgs.writeShellScriptBin "file-open" ''exec xdg-open "$@"'')
    ];
  };
}
