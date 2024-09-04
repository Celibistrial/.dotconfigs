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
    wl-clipboard
    waybar
  ];
}
