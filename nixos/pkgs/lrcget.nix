{
  stdenv,
  lib,
  dpkg,
  fetchurl,
  autoPatchelfHook,
  glib-networking,
  openssl,
  webkitgtk,
  wrapGAppsHook,
  libsoundio,
  x11Support ? true,
  libX11,
  wrapGAppsHook3,
}:
stdenv.mkDerivation rec {
  name = "lrcget";
  version = "0.3.2";

  src = fetchurl {
    url = "https://github.com/tranxuanthang/lrcget/releases/download/0.3.2/lrcget_0.3.2_ubuntu_22.04_amd64.deb";
    sha256 = "sha256-+3ryA8mcK/F34wGVUKcx9QirfaV5/Px7rt3yekAA8N0=";
  };
  postFixup = ''

    wrapProgram "$out/bin/lrcget" \
      --set WEBKIT_DISABLE_COMPOSITING_MODE 1

    # libX11.s# o is loaded dynamically so autopatch doesn't detect it
    # patchelf \
    #   --add-needed ${libX11}/lib/libX11.so \
    #   $out/bin/lrcget
  '';
  nativeBuildInputs = [
    wrapGAppsHook3
    autoPatchelfHook
    dpkg
  ];

  buildInputs = [
    glib-networking
    openssl
    libsoundio
    webkitgtk
    wrapGAppsHook
  ];

  unpackCmd = "dpkg-deb -x $curSrc source";

  installPhase = "mv usr $out";
}
