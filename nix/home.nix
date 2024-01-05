{config, pkgs, ...}:
let
  multi-i3status = (pkgs.callPackage ~/dotfiles/nix/multi-i3stauts.nix pkgs);
in
{

  imports = [
    ~/.config/home-manager/local.nix
  ];

  nixpkgs.config.allowUnfree = true;

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  services.picom.enable = true;


  home.packages = with pkgs; [
    gcc
    gnumake
    libtool
    cmake
    glib
    slack
    neovim
    python310
    xsel
    xclip
    evince
    sshfs
    gnome.gnome-screenshot
    gnome3.dconf-editor
    openssl
    pavucontrol
    discord
    zathura
    gzip
    libnotify
    screen
    espeak-ng
    turbovnc
    baobab
    tmux
    wezterm
    vifm
    vlc
    translate-shell
    neofetch
    direnv
    deadd-notification-center
    bottom
    patchelf
    zulip
    aspell
    aspellDicts.en
    multi-i3status
  ];

  # EDITOR
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
    extraPackages = epkgs: [
      epkgs.vterm
    ];
  };

  # bash
  programs.bash = {
    enable = true;
    sessionVariables = {
      NIX_SHELL_PRESERVE_PROMPT=1;
    };
    initExtra = ''
               if [ -e ~/dotfiles/bashrc ]; then
                 source ~/dotfiles/bashrc
               fi
               
               # disable middle mouse pointer
               function disable_embedded(){
                  for id in $(xinput | grep TPPS | grep -oP '(?<=id=)\d*')
                  do
                      xinput disable $id
                  done
               }
               function enable_embedded(){
                  for id in $(xinput | grep TPPS | grep -oP '(?<=id=)\d*')
                  do
                      xinput enable $id
                  done
               }

               # Refer from https://wiki.vifm.info/index.php/How_to_set_shell_working_directory_after_leaving_Vifm
               vicd()
               {
                   local dst="$(command vifm --choose-dir - "$@")"
                       if [ -z "$dst" ]; then
                               echo 'Directory picking cancelled/failed'
                                       return 1
                                           fi
                                               cd "$dst"
               }
               export EDITOR=nvim
               eval "$(direnv hook bash)"
    '';
  };

  # git
  programs.git = {
    enable = true;
    userName = "Endered";
    userEmail = "yy56ga10ve@gmail.com";
  };

  home.file = {
    ".config/i3/config" = {
      source = ~/dotfiles/config/i3/config;
    };
    ".config/i3status/config" = {
      source = ~/dotfiles/config/i3status/config;
    };
    ".vimrc" = {
      source = ~/dotfiles/vimrc;
    };
    ".config/nvim/init.vim" = {
      source = ~/dotfiles/init.vim;
    };
    ".config/nvim/dein.toml" = {
      source = ~/dotfiles/dein.toml;
    };
    ".config/nvim/dein_lazy.toml" = {
      source = ~/dotfiles/dein_lazy.toml;
    };
    ".emacs.d/init.el" = {
      text = ''
             (load "~/dotfiles/init.el")
'';
    };
    ".wezterm.lua" = {
      source = ~/dotfiles/wezterm.lua;
    };
    ".vifm/vifmrc" = {
      source = ~/dotfiles/vifmrc;
    };
    ".config/discord/settings.json" = {
      text = ''
           {
               "SKIP_HOST_UPDATE": true
           }
'';
    };
    ".config/gtk-3.0/settings.ini" = {
      text = ''
           [Settings]
           gtk-application-prefer-dark-theme = true
'';
    };
    ".background-image" = {
      text = ''
             P3
             1 1
             255
             78 4 87
'';
    };
    ".config/tmux/tmux.conf" = {
      text = ''
             set -g default-terminal "tmux-256color"
             set -ag terminal-overrides ",xterm-256color:RGB"
'';
    };
    ".config/deadd/deadd.conf" = {
      source = ~/dotfiles/config/deadd/deadd.conf;
    };
    ".config/deadd/deadd.css" = {
      source = ~/dotfiles/config/deadd/deadd.css;
    };
  };

  programs.home-manager.enable = true;
}
