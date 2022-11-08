final: prev: efinal: eprev:
eprev.flymake-collection.overrideAttrs ({patches ? [], ...}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/mohkale/flymake-collection/pull/11.diff";
        hash = "sha256-QM9rzprfYXvyD4GkBxaGOuZgv/fP+15sqeDdgyzLUbc=";
      }
    ];
})
