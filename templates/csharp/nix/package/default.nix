{
  buildDotnetModule,
  dotnetCorePackages,
}:
buildDotnetModule {
  pname = "package";
  version = "0.0.1";
  src = ./../..;
  projectFile = "package.sln";
  nugetDeps = ./deps.json;
  dotnet-sdk = dotnetCorePackages.sdk_8_0;
  dotnet-runtime = dotnetCorePackages.runtime_8_0;
  runtimeDeps = [];
}
