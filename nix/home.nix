{config, pkgs, ...}:
{

  nixpkgs.config.allowUnfree = true;

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";


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
    dunst
    libnotify
    screen
    espeak-ng
    tigervnc
    baobab
    tmux
    wezterm
    vifm
    vlc
    translate-shell
    neofetch
    direnv
  ];

  # EDITOR
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.vterm
    ];
  };

  xresources.properties = {
    "Xft.dpi" = 120;
  };

  # bash
  programs.bash = {
    enable = true;
    sessionVariables = {
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
               neofetch
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
  };

  programs.home-manager.enable = true;
}
