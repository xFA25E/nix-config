final: prev: let
  pname = "cursor";
  version = "3.1.17";
  urlHash = "fce1e9ab7844f9ea35793da01e634aa7e50bce90";
in
  prev.code-cursor.overrideAttrs (_: {
    inherit version;
    sourceRoot = "${pname}-${version}-extracted/usr/share/cursor";
    src = final.appimageTools.extract {
      inherit version pname;
      src = final.fetchurl {
        url = "https://downloads.cursor.com/production/${urlHash}/linux/x64/Cursor-${version}-x86_64.AppImage";
        hash = "sha256-+Pk5MvQSjhoKJdtN+pWX/vcyWnHmbXvJ5wFDvgtHo20=";
      };
    };

    preInstall = "mkdir -p bin";
  })
