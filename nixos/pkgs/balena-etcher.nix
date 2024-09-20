{
  lib,
  appimageTools,
  fetchurl,
  makeDesktopItem,
}:
appimageTools.wrapType2 rec {
  pname = "balena-etcher";
  version = "1.19.21";
  src = fetchurl {
    url = "https://github.com/balena-io/etcher/releases/download/v${version}/balenaEtcher-${version}-x64.AppImage";
    hash = "sha256-dHhz7vcrrJZu4rWXRtwcIt2UvFThZrtKHz9H2qV2H60=";
  };
  extraPkgs = pkgs: with pkgs; [libthai];

  desktopItem = makeDesktopItem {
    name = "balena-etcher";
    exec = "balena-etcher";
    terminal = false;
    desktopName = "Balena Etcher";
  };
  extraInstallCommands = ''
    mkdir -p $out/share/applications
    install -Dm644 ${desktopItem}/share/applications/* $out/share/applications
  '';
}
