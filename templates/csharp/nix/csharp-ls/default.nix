{
  buildDotnetModule,
  dotnetCorePackages,
  fetchFromGitHub,
}: let
  version = "0.6.0";
in
  buildDotnetModule {
    inherit version;
    pname = "csharpLanguageServer";

    src = fetchFromGitHub {
      owner = "razzmatazz";
      repo = "csharp-language-server";
      rev = version;
      hash = "sha256-cGy09Q8wzQBH65n2zzIrmqOMkXtq6ylbwe6/tMbVNWw=";
    };

    projectFile = "src/csharp-language-server.sln";
    nugetDeps = ./deps.nix;

    dotnet-sdk = dotnetCorePackages.sdk_7_0;
    dotnet-runtime = dotnetCorePackages.runtime_7_0;
  }
