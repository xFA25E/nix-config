final: prev: efinal: eprev:
eprev.pcmpl-args.overrideAttrs ({patches ? [], ...}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/xFA25E/pcmpl-args.el/commit/fdc51e554160963fb2f4c9ce0041822e1515d1e3.diff";
        sha256 = "0cscz3npna668ywp7302j5f3qrpk4xy5i8f347nv3rwrjhkmwh12";
      }
    ];
})
