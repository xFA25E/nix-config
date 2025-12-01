final: prev: let
  pname = "cursor";
  version = "2.1.32";
  urlHash = "ef979b1b43d85eee2a274c25fd62d5502006e425";
in
  prev.code-cursor.overrideAttrs (_: {
    inherit version;
    sourceRoot = "${pname}-${version}-extracted/usr/share/cursor";
    src = final.appimageTools.extract {
      inherit version pname;
      src = final.fetchurl {
        url = "https://downloads.cursor.com/production/${urlHash}/linux/x64/Cursor-${version}-x86_64.AppImage";
        hash = "sha256-CKLUa5qaT8njAyPMRz6+iX9KSYyvNoyLZFZi6wmR4g0=";
      };
    };

    preInstall = "mkdir -p bin";
  })
