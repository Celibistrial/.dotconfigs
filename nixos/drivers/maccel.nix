{
  stdenv,
  klibcShrunk,
  lib,
  fetchFromGitHub,
  kernel,
}:
stdenv.mkDerivation rec {
  pname = "maccel";
  version = "0.2.1";

  src = fetchFromGitHub {
    owner = "Gnarus-G";
    repo = "maccel";
    rev = "v${version}";
    hash = "sha256-j6rxNGhRRah6RkZc0KORJfwSgpSQONMK2LgZ5WBMWxA=";
  };

  sourceRoot = "source/";
  hardeningDisable = ["pic" "format"];
  prePatch = ''
    substituteInPlace Makefile \
      --replace "sudo" ""
  '';
  nativeBuildInputs = [
    kernel.moduleBuildDependencies
    klibcShrunk
  ];
  makeFlags = [
    "KDIR=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    "MODULEDIR=$out/lib/modules/${kernel.modDirVersion}/kernel/drivers/usb"
    "DRIVERDIR=${src}/driver/"
    "CC=${stdenv.cc.targetPrefix}gcc"
  ];
  meta = {
    description = "Mouse acceleration driver and kernel module for Linux.";
    homepage = "https://www.maccel.org/";
    license = lib.licenses.gpl2;
    platforms = lib.platforms.linux;
  };
}
