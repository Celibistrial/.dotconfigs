{
  stdenv,
  lib,
  fetchurl,
  nix-update-script,
}:
stdenv.mkDerivation rec {
  pname = "proton-ge-custom";
  version = "GE-Proton9-11";

  src = fetchurl {
    url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
    hash = "sha256-vpr4DVWI7TZWb+roijpX5kJRPOJT4wZhT/ZnRrkgZKI=";
  };

  buildCommand = ''
    runHook preBuild

    mkdir -p $out/bin
    tar -C $out/bin --strip=1 -x -f $src

    runHook postBuild
  '';

  passthru.updateScript = nix-update-script {};

  meta = with lib; {
    description = "Compatibility tool for Steam Play based on Wine and additional components";
    homepage = "https://github.com/GloriousEggroll/proton-ge-custom";
    license = licenses.bsd3;
    platforms = ["x86_64-linux"];
    maintainers = with maintainers; [ataraxiasjel];
    preferLocalBuild = true;
  };
}
