self: super: let
  inherit (super) fetchFromGitHub fetchzip runCommand writeText;
  inherit (super.lib.strings) makeSearchPath concatMapStringsSep concatStringsSep;
  inherit (super.lib.trivial) importJSON;
  inherit (super.stdenv) mkDerivation;

  slynk = fetchFromGitHub {
    name = "slynk";
    owner = "joaotavora";
    repo = "sly";
    rev = "eb67be9698794ba66a09f46b7cfffab742863a91";
    sha256 = "11yclc8i6gpy26m1yj6bid6da22639zpil1qzj87m5gfvxiv4zg6";
  };
  slynk-systems = [
    "slynk"
    "slynk/arglists"
    "slynk/fancy-inspector"
    "slynk/package-fu"
    "slynk/mrepl"
    "slynk/trace-dialog"
    "slynk/profiler"
    "slynk/stickers"
    "slynk/indentation"
    "slynk/retro"
  ];

  swm-config = runCommand "swm-config" { src = ./swm-config; } ''
    mkdir $out
    cp $src/*.lisp $src/*.asd $out/
  '';
  dependencies = (map fetchzip (importJSON ./dependency-urls.json)) ++ [ swm-config slynk ];
  systems-to-load = [ "swm-config" ] ++ slynk-systems;
  load-stumpwm = writeText "load-stumpwm.lisp" ''
    (require "asdf")
    (mapc #'asdf:load-system (list ${concatMapStringsSep " " (item: "\"${item}\"") systems-to-load}))
  '';
in {
  stumpwm = mkDerivation {
    pname = "stumpwm";
    version = "20.11";
    src = fetchFromGitHub {
      name = "stumpwm";
      owner = "stumpwm";
      repo = "stumpwm";
      rev = "20.11";
      sha256 = "1ghs6ihvmb3bz4q4ys1d3h6rdi96xyiw7l2ip7jh54c25049aymf";
    };
    nativeBuildInputs = with self; [ sbcl autoconf texinfo makeWrapper pkgconfig ];
    configurePhase = ''
      ./autogen.sh
      ./configure --prefix=$out --with-module-dir=$out/share/stumpwm/modules
    '';
    preBuild = ''
      mkdir dependencies
      cp -rL ${concatStringsSep " " dependencies} dependencies/
      chmod -R 777 dependencies
      export CL_SOURCE_REGISTRY="$PWD:$PWD/dependencies//:"
      export ASDF_OUTPUT_TRANSLATIONS="(:output-translations :ignore-inherited-configuration (t \"$PWD/.common-lisp/\"))"
      cat "${load-stumpwm}" >./load-stumpwm.lisp
    '';
    dontStrip = true;
  };
}
