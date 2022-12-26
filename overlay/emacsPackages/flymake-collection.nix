final: prev: efinal: eprev:
eprev.flymake-collection.overrideAttrs ({patches ? [], ...}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/mohkale/flymake-collection/pull/12.diff";
        hash = "sha256-kxaZiM65w+qK1fOLiUQ6Hd3Ue4Qjfe5c59+iOFyrP+g=";
      }
      {
        url = "https://github.com/xFA25E/flymake-collection/commit/00319c59b78f054fd39806ed58c5df0bdecf0be2.diff";
        hash = "sha256-ZXDAkrTAVmBuK/7c4LbGAm1+YzuvDcSRxgGvVs2Reok=";
      }
    ];
})
