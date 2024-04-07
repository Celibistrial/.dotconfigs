{
  config,
  pkgs,
  ...
}
: {
  home.username = "gaurav";
  home.homeDirectory = "/home/gaurav";
  home.stateVersion = "23.11";
  home.sessionVariables = {
    # MOZ_DISABLE_RDD_SANDBOX = 1;
  };

  programs.home-manager.enable = true;
  home.packages = with pkgs; [
    rofimoji
  ];
  programs.rofi = {
    enable = true;
    theme = "Arc-Dark";
    plugins = with pkgs; [
      rofi-calc
    ];
    extraConfig = {
      modi = "calc,combi,drun,run,ssh";
    };
  };
  programs = {
    direnv = {
      enable = true;
      enableZshIntegration = true; # see note on other shells below
      nix-direnv.enable = true;
    };
  };
  services.mpris-proxy.enable = true;
  home.pointerCursor = {
    x11.enable = true;
    name = "Bibata-Modern-Classic";
    package = pkgs.bibata-cursors;
    size = 20;
    gtk.enable = true;
  };
  gtk.enable = true;
  programs.tmux = {
    enable = true;
    plugins = with pkgs.tmuxPlugins; [
      sensible
      vim-tmux-navigator
      catppuccin
      yank
    ];
    prefix = "C-Space";
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

  # gtk.cursorTheme.package = pkgs.bibata-cursors;
  # gtk.cursorTheme.name = "Bibata-Modern-Classic";

  gtk.theme.package = pkgs.catppuccin-gtk;
  gtk.theme.name = "Catppuccin-Frappe-Standard-Blue-Dark";

  gtk.iconTheme.package = pkgs.papirus-icon-theme;
  gtk.iconTheme.name = "Papirus";
}
