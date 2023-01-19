{
  alejandra,
  csharp-ls,
  dos2unix,
  dotnetCorePackages,
  dotnetPackages,
  libxml2,
  mkShell,
  statix,
}:
mkShell {
  packages = [
    alejandra
    csharp-ls
    dos2unix
    dotnetCorePackages.sdk_7_0
    dotnetPackages.Nuget
    libxml2.bin
    statix
  ];
}
