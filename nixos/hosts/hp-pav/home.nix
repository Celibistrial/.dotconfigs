{
  config,
  lib,
  pkgs,
  inputs,
  ...
}
: {
  catppuccin = {
    enable = true;
    flavor = "mocha";
    accent = "blue";
  };

  home = {
    username = "gaurav";
    homeDirectory = "/home/gaurav";
    stateVersion = "23.11";
    packages = with pkgs; [
      rofimoji
    ];
    pointerCursor = lib.mkForce {
      x11.enable = true;
      name = "Bibata-Modern-Classic";
      package = pkgs.bibata-cursors;
      size = 22;
      gtk.enable = true;
    };
  };
  programs = {
    zoxide.enable = true;
    git = {
      enable = true;
      userName = "Celibistrial";
      userEmail = "ryan80222@gmail.com";
      extraConfig = {
        commit.gpgsign = false;
        user.signingkey = "E577B32870E99F38";
      };
    };

    bat.enable = true;
    btop.enable = true;
    btop.package = pkgs.btop.override {
      cudaSupport = true;
    };
    ranger.enable = true;

    home-manager.enable = true;
    rofi = {
      enable = true;
      catppuccin.enable = false;

      theme = ./../../../rofi/catpuccin-mocha.rasi;
      plugins = with pkgs; [
        rofi-calc
      ];
      extraConfig = {
        modi = "calc,combi,drun,run,ssh";
        show-icons = true;
      };
    };
    ssh = {
      enable = true;
      matchBlocks = {
        m35 = {
          hostname = "m35";
          port = 8022;
          user = "u0_a339";
        };
        drop = {
          hostname = "drop";
          user = "root";
        };
        deb1 = {
          hostname = "deb1";
        };
      };
    };

    direnv = {
      enable = true;
      enableZshIntegration = true; # see note on other shells below
      nix-direnv.enable = true;
    };
    tmux = {
      enable = false;
      plugins = with pkgs.tmuxPlugins; [
        sensible
        vim-tmux-navigator
        yank
      ];
      # prefix = "C-Space";
      mouse = true;
      extraConfig = ''
        set-option -sa terminal-overrides ",xterm*:Tc"
        set -g base-index 1
        set -g pane-base-index 1
        set-window-option -g pane-base-index 1
        # set vi-mode
        set-window-option -g mode-keys vi
        # keybindings
        bind-key -T copy-mode-vi v send-keys -X begin-selection
        bind-key -T copy-mode-vi C-v send-keys -X rectangle-toggle
        bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cancel
        bind '"' split-window -v -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
      '';
    };
  };
  services = {
    easyeffects.enable = true;
    mpris-proxy.enable = true;
    gpg-agent = {
      enable = true;
    };
    redshift = {
      enable = true;
      provider = "manual";
      latitude = 28.0;
      longitude = 77.0;
    };
  };
  gtk = {
    enable = true;
    # gtk.cursorTheme.package = pkgs.bibata-cursors;
    # gtk.cursorTheme.name = "Bibata-Modern-Classic";

    #theme.package = pkgs.catppuccin-gtk;
    #theme.name = "Catppuccin-Frappe-Standard-Blue-Dark";
    catppuccin = {
      enable = true;
      flavor = "mocha";
      accent = "blue";
      # size = "standard";
      tweaks = ["normal"];
    };
    # theme = {
    #   package = pkgs.gnome.gnome-themes-extra;
    #   name = "Adwaita-dark";
    # };

    iconTheme = lib.mkForce {
      package = pkgs.papirus-icon-theme;
      name = "Papirus";
    };
  };
}
