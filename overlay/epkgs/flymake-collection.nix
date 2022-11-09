final: prev: efinal: eprev:
eprev.flymake-collection.overrideAttrs ({patches ? [], ...}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/mohkale/flymake-collection/pull/11.diff";
        hash = "sha256-QM9rzprfYXvyD4GkBxaGOuZgv/fP+15sqeDdgyzLUbc=";
      }
      {
        url = "https://github.com/xFA25E/flymake-collection/commit/e36ba5b82fee8661f9254b91049fb76540302e1f.diff";
        hash = "sha256-fBrMTD/tF6HdhRzaC85kwI0qoC6uVolQmbWwl6/kzjE=";
      }
    ];
})
