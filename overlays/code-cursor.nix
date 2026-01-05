final: prev: let
  pname = "cursor";
  version = "2.3.21";
  urlHash = "68e0a0385b87408d050869ea543e3778ad53f78a";
in
  prev.code-cursor.overrideAttrs (_: {
    inherit version;
    sourceRoot = "${pname}-${version}-extracted/usr/share/cursor";
    src = final.appimageTools.extract {
      inherit version pname;
      src = final.fetchurl {
        url = "https://downloads.cursor.com/production/${urlHash}/linux/x64/Cursor-${version}-x86_64.AppImage";
        hash = "sha256-yXMgMi0UTCv+eOu9uNMl1nIjb4bAY5m0LgCS7rjfs+E=";
      };
    };

    preInstall = "mkdir -p bin";
  })
