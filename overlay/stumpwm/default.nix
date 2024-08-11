{
  autoconf,
  emacs,
  lib,
  lispPackages_new,
  makeWrapper,
  pkg-config,
  sbcl,
  src,
  stdenv,
  texinfo,
  writeText,
  withSlynk ? false,
}: let
  l = lib // builtins;

  slynk = sbcl.buildASDFSystem {
    inherit (emacs.pkgs.sly) version src;
    pname = "slynk";
    systems = [
      "slynk"
      "slynk/arglists"
      "slynk/fancy-inspector"
      "slynk/indentation"
      "slynk/mrepl"
      "slynk/package-fu"
      "slynk/profiler"
      "slynk/retro"
      "slynk/stickers"
      "slynk/trace-dialog"
    ];
  };

  stumpwm = sbcl.buildASDFSystem {
    inherit src;
    pname = "stumpwm";
    version = "23.11";
    lispLibs = l.attrsets.attrVals ["alexandria" "cl-ppcre" "clx"] sbcl.pkgs;
    systems = ["dynamic-mixins-swm" "stumpwm"];
  };

  swm-config = sbcl.buildASDFSystem {
    pname = "swm-config";
    version = "0.0.1";
    src = l.sources.sourceFilesBySuffices ./swm-config [".lisp" ".asd"];
    lispLibs =
      [stumpwm]
      ++ l.attrsets.attrVals [
        "alexandria"
        "chronicity"
        "cl-ppcre"
        "jonathan"
        "local-time"
        "local-time-duration"
        "trivia"
      ]
      sbcl.pkgs;
  };

  sbclWithSWMConfig = sbcl.withPackages (_: [swm-config] ++ l.lists.optional withSlynk slynk);

  load-stumpwm = writeText "load-stumpwm.lisp" (''
      (require "asdf")
      (asdf:load-system "swm-config") ''
    + l.strings.optionalString withSlynk ''
      (asdf:load-system "slynk")
      (mapc #'asdf:load-system (remove "slynk/" (asdf:registered-systems) :test-not #'uiop:string-prefix-p))
    '');
in
  stdenv.mkDerivation {
    inherit src;
    name = "stumpwm-with-config";
    nativeBuildInputs = [sbclWithSWMConfig autoconf texinfo makeWrapper pkg-config];
    configurePhase = ''
      ./autogen.sh
      ./configure --prefix=$out --with-module-dir=$out/share/stumpwm/modules
    '';
    preBuild = ''
      makeFlagsArray+=(sbcl_BUILDOPTS="--non-interactive --eval \"(setf sb-impl::*default-external-format* :UTF-8)\" --load ./load-stumpwm.lisp --load ./make-image.lisp")
      cat "${load-stumpwm}" >./load-stumpwm.lisp
    '';
    dontStrip = true;
  }
