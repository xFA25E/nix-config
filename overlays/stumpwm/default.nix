{
  autoconf,
  emacs,
  fetchFromGitHub,
  lib,
  makeWrapper,
  pkg-config,
  sbcl,
  src,
  stdenv,
  texinfo,
  writeText,
  withSlynk ? false,
  SDL2,
  SDL2_ttf,
  libffi,
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
    version = "24.11";
    lispLibs = l.attrsets.attrVals ["alexandria" "cl-ppcre" "clx"] sbcl.pkgs;
    systems = ["dynamic-mixins-swm" "stumpwm"];
  };

  sdl-fonts = sbcl.buildASDFSystem {
    pname = "sdl-fonts";
    version = "0.0.1";
    src = fetchFromGitHub {
      owner = "stumpwm";
      repo = "stumpwm-contrib";
      rev = "78b574de489fa4a4c156d15a55ac991739a9c58f";
      hash = "sha256-TAgfrhjy3WUd6sMR62/5HUFTRIRZdH62kbfqdv1+0gk=";
    };
    nativeLibs = [SDL2 SDL2_ttf];
    lispLibs = [stumpwm] ++ l.attrsets.attrVals ["cffi" "cffi-libffi"] sbcl.pkgs;
    systems = ["sdl-fonts"];
  };

  swm-config = sbcl.buildASDFSystem {
    pname = "swm-config";
    version = "0.0.1";
    src = l.sources.sourceFilesBySuffices ./swm-config [".lisp" ".asd"];
    lispLibs =
      [stumpwm sdl-fonts]
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
      (asdf:load-system "swm-config")
    ''
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
    postInstall = ''
      wrapProgram $out/bin/stumpwm --prefix LD_LIBRARY_PATH : "${lib.strings.makeLibraryPath [SDL2 SDL2_ttf libffi]}"
    '';
    dontStrip = true;
  }
