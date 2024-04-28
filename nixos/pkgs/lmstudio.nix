{
  appimageTools,
  fetchurl,
  lib,
}: let
  pname = "lmstudio";
  version = "0.2.20";

  src = fetchurl {
    url = "https://releases.lmstudio.ai/linux/0.2.20/beta/LM_Studio-0.2.20.AppImage";
    hash = "sha256-T92ZDqGvxJfBkAWsK8EgHdQZnLefK3gDP2vCTL8X+eM=";
  };
in
  appimageTools.wrapType2 {
    inherit pname version src;

    extraInstallCommands = ''
      mv $out/bin/${pname}-${version} $out/bin/${pname}
    '';

    extraPkgs = pkgs: with pkgs; [libsecret];

    meta = with lib; {
      description = "A GUI tool to run LLMs";
      homepage = "https://lmstudio.ai/";
      license = licenses.unfree;
      platforms = ["x86_64-linux" "aarch64-darwin"];
      mainProgram = pname;
    };
  }
