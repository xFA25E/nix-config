final: prev: efinal: eprev:
eprev.flymake-collection.overrideAttrs ({patches ? [], ...}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/xFA25E/flymake-collection/commit/00319c59b78f054fd39806ed58c5df0bdecf0be2.diff";
        hash = "sha256-ZXDAkrTAVmBuK/7c4LbGAm1+YzuvDcSRxgGvVs2Reok=";
      }
    ];
})
