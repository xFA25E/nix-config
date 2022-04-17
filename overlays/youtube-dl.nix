self: super: {
  youtube-dl = super.youtube-dl.overrideAttrs (old: {
    patches = old.patches ++ [
      (super.fetchpatch {
        name = "fix-n-extractor.patch";
        url = "https://github.com/ytdl-org/youtube-dl/commit/a0068bd6bec16008bda7a39caecccbf84881c603.diff";
        sha256 = "1b03fz3jx719zzsyrzy93gg4lyhmdjia73kkv9dfp9i3rsg88axm";
      })
    ];
  });
}
