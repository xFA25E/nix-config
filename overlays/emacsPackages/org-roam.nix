final: prev: efinal: eprev:
eprev.org-roam.overrideAttrs ({patches ? [], ...}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/xFA25E/org-roam/commit/1e9620d21a952105c7baf6d7a15fba0ec654aafa.diff";
        hash = "sha256-KvOxBCpweDyQvCYOUFH8tu9VG/WlSck5vlQXsDr2O9I=";
      }
    ];
})
