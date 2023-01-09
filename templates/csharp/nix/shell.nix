{
  alejandra,
  csharp-ls,
  dos2unix,
  dotnetCorePackages,
  dotnetPackages,
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
    statix
  ];
}
