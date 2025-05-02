final: prev: efinal: eprev: let
  rev = "637d40cae658e69f3d65cd0af4f5077da61d7f71";
in
  efinal.melpaBuild {
    version = "0.1";
    pname = "discomfort";
    src = final.fetchgit {
      inherit rev;
      url = "https://codeberg.org/xFA25E/discomfort.git";
      hash = "sha256-/h50P2gji1WAtRIalRUzuJfOpa7qRRsFuEMmAACq5lo=";
    };
    commit = rev;
    recipe = final.writeText "recipe" ''
      (discomfort :fetcher git :url "https://codeberg.org/xFA25E/discomfort.git")
    '';
    packageRequires = [efinal.debase];
  }
