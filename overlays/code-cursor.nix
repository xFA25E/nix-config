final: prev: let
  pname = "cursor";
  version = "3.29.15";
  urlHash = "86776fcb52ca505783155631089b65d646f9b03f";
in
  prev.code-cursor.overrideAttrs (_: {
    inherit version;
    sourceRoot = "${pname}-${version}-extracted/usr/share/cursor";
    src = final.appimageTools.extract {
      inherit version pname;
      src = final.fetchurl {
        url = "https://downloads.cursor.com/production/${urlHash}/linux/x64/Cursor-${version}-x86_64.AppImage";
        hash = "sha256-rvFuzyfb6ohiXQbU+UXG0RUsSJ8v7dUDLJeNaW+UF9E=";
      };
    };

    preInstall = "mkdir -p bin";
  })
