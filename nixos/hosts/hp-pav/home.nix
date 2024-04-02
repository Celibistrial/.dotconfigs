{
  config,
  pkgs,
  ...
}
: {
  home.username = "gaurav";
  home.homeDirectory = "/home/gaurav";
  home.stateVersion = "23.11";

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

  # gtk.cursorTheme.package = pkgs.bibata-cursors;
  # gtk.cursorTheme.name = "Bibata-Modern-Classic";

  gtk.theme.package = pkgs.catppuccin-gtk;
  gtk.theme.name = "Catppuccin-Frappe-Standard-Blue-Dark";

  gtk.iconTheme.package = pkgs.papirus-icon-theme;
  gtk.iconTheme.name = "Papirus";
}
