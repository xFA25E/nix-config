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
        url = "https://github.com/xFA25E/sly/commit/4f95f882a7179170c09074c5c6986b407caa60f1.diff";
        hash = "sha256-PgCGRHOyAuz9QMbc0EyX66/m29h+7xrhRycDwD0TDA0=";
      }
    ];
})
