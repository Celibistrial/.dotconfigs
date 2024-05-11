# Download LLMs per api
# curl http://localhost:11434/api/pull -d '{ "name": "llama2" }'
{
  config,
  lib,
  pkgs,
  ...
}: let
in {
  system.activationScripts = {
    script.text = ''
      install -d -m 755 /home/gaurav/.open-webui/data -o root -g root
    '';
  };
  virtualisation = {
    podman = {
      enable = true;
      dockerCompat = true;
      #defaultNetwork.settings.dns_enabled = true;
    };

    oci-containers = {
      backend = "podman";

      containers = {
        open-webui = {
          autoStart = true;
          image = "ghcr.io/open-webui/open-webui:main";

          environment = {
            "TZ" = "Asia/Kolkata";
            "OLLAMA_API_BASE_URL" = "http://127.0.0.1:11434/api";
            "OLLAMA_BASE_URL" = "http://127.0.0.1:11434";
            "WEBUI_SECRET_KEY" = "VHauLdSV9$NPxyUChzA9@YyYDR!WPj";
          };

          volumes = [
            "/home/gaurav/.open-webui/data:/app/backend/data"
          ];

          ports = [
            "127.0.0.1:3000:8080" # Ensures we listen only on localhost
          ];

          extraOptions = [
            "--pull=newer" # Pull if the image on the registry is newer
            "--name=open-webui"
            "--hostname=open-webui"
            "--network=host"
            "--add-host=host.containers.internal:host-gateway"
          ];
        };
      };
    };
  };
}
