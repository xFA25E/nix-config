self: super: {
  youtube-dl = super.youtube-dl.overrideAttrs (old: {
    patches = old.patches ++ [
      (super.fetchpatch {
        name = "fix-n-extractor.patch";
        url = "https://github.com/ytdl-org/youtube-dl/commit/a0068bd6bec16008bda7a39caecccbf84881c603.diff";
        sha256 = "1b03fz3jx719zzsyrzy93gg4lyhmdjia73kkv9dfp9i3rsg88axm";
      })
      (super.fetchpatch {
        name = "cozytv-extractor.patch";
        url = "https://github.com/xFA25E/youtube-dl/commit/53ab8d643241250bc789ab0e2283531256097f77.diff";
        sha256 = "1wv6ipl18h8x6af4vz0ykiadxf071mvq1qk9p5vy2g3mcmx0r4mr";
      })
    ];
  });
}
