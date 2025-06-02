# Found a hack to find avalonia compiled libraries
# https://www.reddit.com/r/NixOS/comments/1cvy0jk/is_it_possible_to_debug_c_avalonia_ui_code_on/
{
  alejandra,
  csharp-ls,
  omnisharp-roslyn,
  dos2unix,
  dotnetCorePackages,
  dotnetPackages,
  libxml2,
  mkShell,
  statix,
  csharpier,
  # AVALONIA
  nix-ld,
  lib,
  stdenv,
  fontconfig,
  xorg,
  avalonia-ilspy,
}:
mkShell {
  packages = [
    alejandra
    csharp-ls
    # omnisharp-roslyn
    dos2unix
    dotnetCorePackages.sdk_8_0
    csharpier
    dotnetPackages.Nuget
    libxml2.bin
    statix
    # AVALONIA
    nix-ld
    avalonia-ilspy
  ];

  # AVALONIA
  NIX_LD_LIBRARY_PATH = lib.makeLibraryPath [stdenv.cc.cc];

  LD_LIBRARY_PATH = lib.makeLibraryPath [
    stdenv.cc.cc
    # libskiasharp
    fontconfig
    xorg.libX11
    xorg.libICE
    xorg.libSM
  ];

  NIX_LD = "${stdenv.cc.libc_bin}/bin/ld.so";

  shellHook = ''
    export PROJECT_ROOT=$PWD

    echo Welcome to devshell at $PROJECT_ROOT
  '';
}
