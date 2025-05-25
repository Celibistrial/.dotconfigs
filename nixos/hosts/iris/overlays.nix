{
  config,
  lib,
  pkgs,
  ...
}: {
  nixpkgs.overlays = [
    (
      final: prev: {
        # picom = prev.picom.overrideAttrs (old: {
        #   src = prev.fetchFromGitHub {
        # owner = "yshui";
        # repo = "picom";
        # rev = "v11.2";
        # hash = "sha256-7ohtI890CutwprPEY5njqWou0fD6T9eu51EBSQ2/lWs=";
        # fetchSubmodules = true;
        # };
        #   nativeBuildInputs = with prev; [
        #     asciidoc
        #     docbook_xml_dtd_45
        #     docbook_xsl
        #     makeWrapper
        #     meson
        #     ninja
        #     pkg-config
        #   ];
        # });
      }
    )
  ];
}
