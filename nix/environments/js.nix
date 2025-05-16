{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.js;
in
{
  options.my-settings.js = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf (!cfg.disable) {
    home.packages = with pkgs; [
      nodejs
      typescript
      typescript-language-server
    ];
  };
}
