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
}
