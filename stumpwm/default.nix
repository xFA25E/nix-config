{ src, pkgs, slynk ? false, sly ? pkgs.emacsPackages.sly.src }: let
  inherit (builtins) filterSource match;
  inherit (pkgs) fetchzip runCommand writeText;
  inherit (pkgs.lib) optionals optionalString;
  inherit (pkgs.lib.strings) concatMapStringsSep;
  inherit (pkgs.lib.trivial) importJSON;

  swm-config = filterSource (p: t: t == "regular" && match ".*\\.(lisp|asd)" p != null) ./swm-config;

  deps = (map fetchzip (importJSON ./dependency-urls.json)) ++ [ swm-config ] ++ optionals slynk [ sly ];

  load-stumpwm = writeText "load-stumpwm.lisp" (''
    (require "asdf")
    (asdf:load-system "swm-config")
  '' + optionalString slynk ''
    (asdf:load-system "slynk")
    (mapc #'asdf:load-system (remove "slynk/" (asdf:registered-systems) :test-not #'uiop:string-prefix-p))
  '');

in pkgs.stdenv.mkDerivation {
  inherit src deps;
  name = "stumpwm";
  nativeBuildInputs = with pkgs; [ sbcl autoconf texinfo makeWrapper pkg-config ];
  configurePhase = ''
    ./autogen.sh
    ./configure --prefix=$out --with-module-dir=$out/share/stumpwm/modules
  '';
  preBuild = ''
    mkdir deps
    cp -rL $deps deps/
    chmod -R 777 deps
    export CL_SOURCE_REGISTRY="$PWD:$PWD/deps//:"
    export ASDF_OUTPUT_TRANSLATIONS="(:output-translations :ignore-inherited-configuration (t \"$PWD/.common-lisp/\"))"
    cat "${load-stumpwm}" >./load-stumpwm.lisp
  '';
  dontStrip = true;
}
