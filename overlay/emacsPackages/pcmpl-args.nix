final: prev: efinal: eprev:
eprev.pcmpl-args.overrideAttrs ({patches ? [], ...}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/xFA25E/pcmpl-args.el/commit/fdc51e554160963fb2f4c9ce0041822e1515d1e3.diff";
        hash = "sha256-IkBeJ5SZ57HtIcOhWHwn82Y8XJECjHO5R8Yoe+34TDM=";
      }
    ];
})
