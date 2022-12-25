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
        url = "https://github.com/xFA25E/mpv-youtube-quality/commit/f1fa3d6d81116d231c62a272aa8cae3badc61f8d.diff";
        hash = "sha256-61Y6D0XC2pKK10zfVM50FxDDYaVbQZXXapxjPoEIen8=";
      }
    ];
  postPatch =
    postPatch
    + ''
      sed -r -i youtube-quality.lua -e 's/"youtube-dl"/"yt-dlp"/g'
    '';
})
