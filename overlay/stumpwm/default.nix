{
  autoconf,
  emacsPackages,
  fetchzip,
  lib,
  makeWrapper,
  pkg-config,
  runCommand,
  sbcl,
  sly ? emacsPackages.sly.src,
  slynk ? false,
  src,
  stdenv,
  texinfo,
  writeText,
}: let
  inherit (lib.lists) optional;
  inherit (lib.sources) sourceFilesBySuffices;
  inherit (lib.strings) concatMapStringsSep optionalString;
  inherit (lib.trivial) importJSON;

  swm-config = sourceFilesBySuffices ./swm-config [".lisp" ".asd"];

  deps =
    (map fetchzip (importJSON ./dependency-urls.json))
    ++ [swm-config]
    ++ optional slynk sly;

  load-stumpwm = writeText "load-stumpwm.lisp" (''
      (require "asdf")
      (asdf:load-system "swm-config")
    ''
    + optionalString slynk ''
      (asdf:load-system "slynk")
      (mapc #'asdf:load-system (remove "slynk/" (asdf:registered-systems) :test-not #'uiop:string-prefix-p))
    '');
in
  stdenv.mkDerivation {
    inherit src deps;
    name = "stumpwm";
    nativeBuildInputs = [sbcl autoconf texinfo makeWrapper pkg-config];
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
