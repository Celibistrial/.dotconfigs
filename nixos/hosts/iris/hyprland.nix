{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.hyprland.enable = true;
  environment.systemPackages = with pkgs; [
    hypridle
    hyprlock
    hyprpaper
    hyprpolkitagent
    wl-clipboard
    waybar
  ];
}
