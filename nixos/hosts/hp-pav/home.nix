{
  config,
  lib,
  pkgs,
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
      size = 20;
      gtk.enable = true;
    };
  };
  programs = {
    emacs = {
      enable = true;
      extraPackages = epkgs: [epkgs.vterm];
    };
    zathura = {
      enable = true;
      extraConfig = ''
        set database sqlite
      '';
    };
    mpv.enable = true;
    bat.enable = true;
    btop.enable = true;
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
  services.mpris-proxy.enable = true;
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
