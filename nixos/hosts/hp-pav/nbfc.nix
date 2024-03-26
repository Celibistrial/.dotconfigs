{
  config,
  pkgs,
  inputs,
  ...
}: let
  myUser = "gaurav"; #adjust this to your username
  command = "bin/nbfc_service --config-file '/home/${myUser}/.config/nbfc.json'";
  # nbfc = pkgs.stdenv.mkDerivation {
  #   name = "nbfc-linux";
  #   version = "0.1.7";
  #   src = pkgs.fetchFromGitHub {
  #     owner = "Celibistrial";
  #     repo = "nbfc-linux";
  #     rev = "3edf9978ca12d71e7dca3ee863d3aef2e492c430";
  #     sha256 = "OSrR05BJOcvzpAw68Bn3nhtAr5M9t42h12nAc+kay5I=";
  #   };
  #   buildFlags = ["PREFIX=$(out)" "confdir=/etc"];
  #   installPhase = let
  #     installFlags = ["PREFIX=$out"];
  #   in ''
  #     make ${builtins.concatStringsSep " " installFlags}\
  #          install-core \
  #          install-client-c\
  #          install-configs\
  #          install-docs\
  #          install-completion
  #   '';
  # };
in {
  environment.systemPackages = with pkgs; [
    nix-prefetch-github
    inputs.nbfc-linux.packages.x86_64-linux.default
  ];
  systemd.services.nbfc_service = {
    enable = true;
    description = "NoteBook FanControl service";
    serviceConfig.Type = "simple"; #related upstream config: https://github.com/nbfc-linux/nbfc-linux/blob/main/etc/systemd/system/nbfc_service.service.in
    path = [pkgs.kmod];
    script = "${inputs.nbfc-linux.packages.x86_64-linux.default}/${command}";
    wantedBy = ["multi-user.target"];
  };
}
