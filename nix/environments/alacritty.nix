{ config, lib, pkgs, ... }:
let
  cfg = config.my-settings.alacritty;
  pure-setting-file = ~/dotfiles/config/alacritty/alacritty.toml;
  font-size = toString cfg.font-size;
  setting-file = pkgs.runCommand "modyfied-alacritty.toml" {} ''
               cat ${pure-setting-file} |
                 sed -e 's/size = .*/size = ${font-size}/' > $out
  '';
in
{
  options.my-settings.alacritty = {
    disable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
    use-appimage = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
    font-size = lib.mkOption {
      default = 11;
      type = lib.types.number;
    };
  };

  config = lib.mkIf (!cfg.disable)
    {
      home.packages = lib.mkIf (!cfg.use-appimage) [ pkgs.alacritty ];
      home.file = {
        ".config/alacritty/alacritty.toml" = {
          source = setting-file;
        };
        ".config/alacritty/monokai.toml" = {
          source = ~/dotfiles/config/alacritty/monokai.toml;
        };
      };
    };
}
