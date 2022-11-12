final: prev: efinal: eprev:
eprev.flymake-collection.overrideAttrs ({patches ? [], ...}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/mohkale/flymake-collection/pull/12.diff";
        hash = "sha256-AYUQkANRVta3MBc9u445Qp1qiy3XcibDfOkWDrYomIE=";
      }
    ];
})
