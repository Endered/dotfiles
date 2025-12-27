{config, pkgs, ...}:
let
  unstable-pkgs = import <nixpkgs-unstable> {};
in
{

  imports = [
    ~/.config/home-manager/local.nix
    ~/dotfiles/nix/environments/cpp.nix
    ~/dotfiles/nix/environments/rust.nix
    ~/dotfiles/nix/environments/latex.nix
    ~/dotfiles/nix/environments/skk.nix
    ~/dotfiles/nix/environments/satysfi.nix
    ~/dotfiles/nix/environments/haskell.nix
    ~/dotfiles/nix/environments/scala.nix
    ~/dotfiles/nix/environments/python.nix
    ~/dotfiles/nix/environments/wezterm.nix
    ~/dotfiles/nix/environments/alacritty.nix
    ~/dotfiles/nix/environments/xresources.nix
    ~/dotfiles/nix/environments/fonts.nix
    ~/dotfiles/nix/environments/senbura.nix
    ~/dotfiles/nix/environments/lua.nix
    ~/dotfiles/nix/environments/distant.nix
    ~/dotfiles/nix/environments/i3.nix
    ~/dotfiles/nix/environments/sway.nix
    ~/dotfiles/nix/environments/wayland.nix
    ~/dotfiles/nix/environments/js.nix
    ~/dotfiles/nix/environments/emacs.nix
  ];

  nixpkgs.config.allowUnfree = true;

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  home.packages = with pkgs; [
    libtool
    slack
    neovim
    evince
    sshfs
    gnome-screenshot
    dconf-editor
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
    vifm
    vlc
    translate-shell
    neofetch
    direnv
    wired
    bottom
    patchelf
    zulip-term
    aspell
    aspellDicts.en
    ripgrep
    fd
    imagemagick
    alacritty
    graphviz
    bc
    pandoc
    dua
    fzf
    unstable-pkgs.tinymist
    unstable-pkgs.typst
    unstable-pkgs.typstyle
    (builtins.getFlake (builtins.toString ../tools/cpu-healz)).packages.x86_64-linux.default
    gforth
    just

    # clipboard
    xsel
    xclip
  ];

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

               if [ -e ~/.bashrc_local ]; then
                 source ~/.bashrc_local
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
    settings.user = {
      name = "Endered";
      email = "yy56ga10ve@gmail.com";
    };
  };

  # color theme
  gtk = {
    enable = true;
    theme.name = "Adwaita-dark";
    theme.package = pkgs.gnome-themes-extra;
    gtk2.extraConfig = ''
       gtk-application-prefer-dark-theme = true
    '';
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
  };

  home.file = {
    ".vimrc" = {
      source = ~/dotfiles/vimrc;
    };
    ".config/nvim/init.lua" = {
      source = ~/dotfiles/config/nvim/init.lua;
    };
    ".config/nvim/lua/config/lazy.lua" = {
      source = ~/dotfiles/config/nvim/lua/config/lazy.lua;
    };
    ".config/nvim/lua/plugins/color-scheme.lua" = {
      source = ~/dotfiles/config/nvim/lua/plugins/color-scheme.lua;
    };
    ".config/nvim/lua/plugins/lsp.lua" = {
      source = ~/dotfiles/config/nvim/lua/plugins/lsp.lua;
    };
    ".config/nvim/lua/plugins/file-tree.lua" = {
      source = ~/dotfiles/config/nvim/lua/plugins/file-tree.lua;
    };
    ".emacs.d/init.el" = {
      text = ''
             (load "~/dotfiles/init.el")

             ;; forth settings
             (add-to-list 'load-path "${pkgs.gforth}/share/emacs/site-lisp")
             (autoload 'forth-mode "gforth.el")
             (setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode) 
             			    auto-mode-alist))
             (autoload 'forth-block-mode "gforth.el")
             (setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode) 
             			    auto-mode-alist))
'';
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
    ".config/deadd/deadd.css" = {
      source = ~/dotfiles/config/deadd/deadd.css;
    };
    ".config/deadd/deadd.yml" = {
      source = ~/dotfiles/config/deadd/deadd.yml;
    };
    ".config/wired/wired.ron" = {
      source = ~/dotfiles/config/wired/wired.ron;
    };
  };

  home.sessionPath = [
    (builtins.getEnv "HOME" + "/bin")
  ];

  programs.home-manager.enable = true;
}
