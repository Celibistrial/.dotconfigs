{
  lib,
  appimageTools,
  fetchurl,
  makeDesktopItem,
}:
appimageTools.wrapType2 rec {
  name = "msty";
  src = fetchurl {
    url = "https://assets.msty.app/Msty_x86_64.AppImage";
    hash = "sha256-4jXqjOWaZyfHu/rGzNJDANdQ89VxMmnx6dpR5GnR2F4=";
  };

  desktopItem = makeDesktopItem {
    name = "msty";
    exec = "msty";
    terminal = false;
    desktopName = "Msty";
  };
  extraInstallCommands = ''
    mkdir -p $out/share/applications
    install -Dm644 ${desktopItem}/share/applications/* $out/share/applications
  '';
}
