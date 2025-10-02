inputs: {
  default = final: prev: let
    inherit (final) callPackage;
    inherit (inputs) stumpwm grobi gmdb2;
  in {
    brave-incognito = callPackage ./brave-incognito.nix {};
    browser = callPackage ./browser.nix {};
    cl-hyperspec = callPackage ./cl-hyperspec.nix {};
    dmenu = import ./dmenu.nix final prev;
    # emacsPackagesFor = import ./emacsPackages final prev;
    extract_eml = callPackage ./extract_eml.nix {};
    filename_put_duration = callPackage ./filename_put_duration.nix {};
    format_seconds = callPackage ./format_seconds.nix {};
    image-dired-external-viewer = callPackage ./image-dired-external-viewer.nix {};
    image_clipboard = callPackage ./image_clipboard.nix {};
    install_keys = callPackage ./install_keys.nix {};
    interval_timer = callPackage ./interval_timer.nix {};
    make_backup = callPackage ./make_backup.nix {};
    mpvi = callPackage ./mpvi.nix {};
    notifiers = callPackage ./notifiers.nix {};
    preparehd = callPackage ./preparehd.nix {};
    qrshow = callPackage ./qrshow.nix {};
    recode_video = callPackage ./recode_video.nix {};
    resize_video = callPackage ./resize_video.nix {};
    select_music_dir = callPackage ./select_music_dir.nix {};
    select_ytdl_fmt = callPackage ./select_ytdl_fmt.nix {};
    sort_videos_by_duration = callPackage ./sort_videos_by_duration.nix {};
    stardicts = callPackage ./stardicts {};
    strip_video = callPackage ./strip_video.nix {};
    stumpwm = callPackage ./stumpwm {
      src = stumpwm;
      withSlynk = true;
    };
    sudo_askpass = callPackage ./sudo_askpass.nix {};
    video_seconds = callPackage ./video_seconds.nix {};
    wallpapers = callPackage ./wallpapers {};
    watch_time = callPackage ./watch_time.nix {};
    ytdl = callPackage ./ytdl.nix {};
    ytdla = callPackage ./ytdla.nix {};
    ytdlap = callPackage ./ytdlap.nix {};
    ytdli = callPackage ./ytdli.nix {};
    ytdlm = callPackage ./ytdlm.nix {};
    ytdlmp = callPackage ./ytdlmp.nix {};
    ytdlp = callPackage ./ytdlp.nix {};

    grobi = prev.grobi.overrideAttrs (_: {
      src = grobi;
      vendorHash = "sha256-3hyI5oHV8qEkIsF6pk1xx1H98Wx+Ug/Z2IswVbzIQLQ=";
      patches = map final.fetchpatch [
        {
          url = "https://github.com/fd0/grobi/pull/30.diff";
          hash = "sha256-eSCCfsRuBdNqqd8EF4lhRXCFS1WzXGeOJiP/+h7p1Vk=";
        }
      ];
    });

    # mahogany = callPackage ./mahogany.nix {};

    gmdb2 = callPackage ./gmdb2.nix {src = gmdb2;};

    code-cursor = let
      pname = "cursor";
      version = "1.7.17";
      urlHash = "34881053400013f38e2354f1479c88c9067039a2";
    in
      prev.code-cursor.overrideAttrs (_: {
        inherit version;
        sourceRoot = "${pname}-${version}-extracted/usr/share/cursor";
        src = final.appimageTools.extract {
          inherit version pname;
          src = final.fetchurl {
            url = "https://downloads.cursor.com/production/${urlHash}/linux/x64/Cursor-${version}-x86_64.AppImage";
            hash = "sha256-OsZiUXWKNLO8sUqielk0kap0DAkMY8OvWYO0KV3iads=";
          };
        };

        preInstall = "mkdir -p bin";
      });
  };
}
