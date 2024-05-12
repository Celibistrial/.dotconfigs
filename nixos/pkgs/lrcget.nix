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
}:
stdenv.mkDerivation rec {
  name = "lrcget";
  version = "0.3.2";

  src = fetchurl {
    url = "https://github.com/tranxuanthang/lrcget/releases/download/0.3.2/lrcget_0.3.2_ubuntu_22.04_amd64.deb";
    sha256 = "sha256-+3ryA8mcK/F34wGVUKcx9QirfaV5/Px7rt3yekAA8N0=";
  };
  postFixup = lib.optionalString ((!stdenv.isDarwin) && x11Support) ''
    # libX11.so is loaded dynamically so autopatch doesn't detect it
    patchelf \
      --add-needed ${libX11}/lib/libX11.so \
      $out/bin/lrcget
  '';
  nativeBuildInputs = [
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
