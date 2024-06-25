{
  config,
  lib,
  pkgs,
  ...
}: {
  system.activationScripts = {
    script.text = ''
      install -d -m 755 /home/gaurav/.open-webui/data -o root -g root
    '';
  };
  virtualisation = {
    podman = {
      enable = true;
      dockerCompat = true;
    };
    oci-containers = {
      backend = "podman";

      containers = {
        silly-tavern = {
          image = "goolashe/sillytavern:latest";

          extraOptions = [
            "--network=host"
          ];

          ports = ["8000"];

          volumes = [
            "/home/gaurav/.config/sillytavern/config/:/home/node/app/config"

            "/home/gaurav/.config/sillytavern/user/:/home/node/app/public/user"
          ];
        };
      };
    };
  };
  networking.firewall.allowedTCPPorts = [
    8000
  ];
}
