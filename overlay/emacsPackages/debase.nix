final: prev: efinal: eprev: let
  rev = "5a728897d332229f7c7cc5beb21f806ee41bc12c";
in
  efinal.melpaBuild {
    version = "0.8";
    pname = "debase";
    src = final.fetchgit {
      inherit rev;
      url = "https://codeberg.org/xFA25E/debase.git";
      hash = "sha256-29vT1E4SfRVuz8BY4D7h4zTPUy/hEAFSdAuWAJ/z3ys=";
    };
    commit = rev;
    recipe = final.writeText "recipe" ''
      (debase :fetcher git :url "https://codeberg.org/xFA25E/debase.git")
    '';
  }
