final: prev: let
  pname = "cursor";
  version = "3.19.13";
  urlHash = "dd066f332fcea7382764400fde902f61920648d5";
in
  prev.code-cursor.overrideAttrs (_: {
    inherit version;
    sourceRoot = "${pname}-${version}-extracted/usr/share/cursor";
    src = final.appimageTools.extract {
      inherit version pname;
      src = final.fetchurl {
        url = "https://downloads.cursor.com/production/${urlHash}/linux/x64/Cursor-${version}-x86_64.AppImage";
        hash = "sha256-B9kDP/TsvE2OmhYEq77ZyiQttho3FQlRa4Ux1RDptQg=";
      };
    };

    preInstall = "mkdir -p bin";
  })
