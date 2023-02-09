final: prev: efinal: eprev:
eprev.sly.overrideAttrs ({
  patches ? [],
  prePatch ? "",
  ...
}: {
  prePatch =
    prePatch
    + ''
      substituteInPlace lib/hyperspec.el --replace \
        '"http://www.lispworks.com/reference/HyperSpec/"' \
        '"file://${final.cl-hyperspec}/HyperSpec/"'
    '';
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/joaotavora/sly/pull/441.diff";
        hash = "sha256-+rwVmrN4A4GY8oI3GiuMXq8JUyd0gT1oqdMA6NtTpnU=";
      }
    ];
})
