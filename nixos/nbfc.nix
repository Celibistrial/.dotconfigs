{
  config,
  pkgs,
  ...
}: let
  myUser = "gaurav"; #adjust this to your username
  command = "bin/nbfc_service --config-file '/home/${myUser}/.config/nbfc.json'";

  nbfc = pkgs.stdenv.mkDerivation {
    name = "nbfc-linux";
    version = "0.1.7";

    src = pkgs.fetchFromGitHub {
      owner = "Celibistrial";
      repo = "nbfc-linux";
      rev = "b656b01d1fa88e42f448032105c2269cac1e3597";
      sha256 = "6AC8BVSDtPchasSx0KJGLEXYsbonumn+aoNOxl3D4c8=";
    };

    buildFlags = ["PREFIX=$(out)" "confdir=/etc"];

    installPhase = let
      installFlags = ["PREFIX=$out"];
    in ''
      make ${builtins.concatStringsSep " " installFlags}\
           install-core \
           install-client-c\
           install-configs\
           install-docs\
           install-completion
    '';
  };
in {
  environment.systemPackages = with pkgs; [
    nix-prefetch-github
    nbfc
  ];

  systemd.services.nbfc_service = {
    enable = true;
    description = "NoteBook FanControl service";
    serviceConfig.Type = "simple"; #related upstream config: https://github.com/nbfc-linux/nbfc-linux/blob/main/etc/systemd/system/nbfc_service.service.in
    path = [pkgs.kmod];
    script = "${nbfc}/${command}";
    wantedBy = ["multi-user.target"];
  };
}
