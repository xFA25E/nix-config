final: prev:
prev.mpvScripts.youtube-quality.overrideAttrs ({
  patches ? [],
  postPatch ? "",
  ...
}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/jgreco/mpv-youtube-quality/commit/80a8540c449c8ba63a8e0361e6f17ffe7ef7aaf2.diff";
        hash = "sha256-wbjGF007jUa2rFsbdlRE3JerQkKBy70LKbjp+IwHzKg=";
      }
      {
        url = "https://github.com/xFA25E/mpv-youtube-quality/commit/253640f433a42fbd557f163d473ffb810efbe22b.diff";
        hash = "sha256-OJzW8cRXqf+JcWSc33ormzfElpBjErj9Du0FzluR8FU=";
      }
    ];
})
