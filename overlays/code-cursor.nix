final: prev: let
  pname = "cursor";
  version = "2.5.22";
  urlHash = "0eda506a36f70f8dc866c1ea642fcaf620090083";
in
  prev.code-cursor.overrideAttrs (_: {
    inherit version;
    sourceRoot = "${pname}-${version}-extracted/usr/share/cursor";
    src = final.appimageTools.extract {
      inherit version pname;
      src = final.fetchurl {
        url = "https://downloads.cursor.com/production/${urlHash}/linux/x64/Cursor-${version}-x86_64.AppImage";
        hash = "sha256-R+2561VenFvaPUW5ZZ/hLS/M65PC2BXIqN2JscrbtP4=";
      };
    };

    preInstall = "mkdir -p bin";
  })
