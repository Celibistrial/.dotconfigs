{
  config,
  pkgs,
  ...
}: let
  myUser = "gaurav";
  command = "bin/nbfc_service --config-file '/home/${myUser}/.config/nbfc.json'";
in {
  environment.systemPackages = with pkgs; [
    nbfc-linux
  ];
  systemd.services.nbfc_service = {
    enable = true;
    description = "NoteBook FanControl service";
    serviceConfig.Type = "simple"; #related upstream config: https://github.com/nbfc-linux/nbfc-linux/blob/main/etc/systemd/system/nbfc_service.service.in
    path = [pkgs.kmod];
    script = "${pkgs.nbfc-linux}/${command}";
    wantedBy = ["multi-user.target"];
  };
}
