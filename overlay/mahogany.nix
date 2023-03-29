# separate builds for speed: wlroots should be separate and maybe other thingies
{
  egl-wayland,
  eudev,
  fetchFromGitHub,
  fetchFromGitLab,
  ffmpeg_4,
  glslang,
  hwdata,
  lib,
  libGL,
  libcap,
  libdrm,
  libinput,
  libpng,
  libxkbcommon,
  lispPackages_new,
  makeWrapper,
  mesa,
  meson,
  ninja,
  pixman,
  pkg-config,
  sbcl,
  seatd,
  stdenv,
  vulkan-loader,
  wayland,
  wayland-protocols,
  wayland-scanner,
  writeText,
  xgboost,
  xorg,
  xwayland,
}: let
  pname = "mahogany";
  version = "0.0.1";

  # SOURCES
  src = fetchFromGitHub {
    owner = "stumpwm";
    repo = "mahogany";
    rev = "e2c03d264ba3d8795ee7b3f278ce9ee2b666c630";
    hash = "sha256-Dse7N8WT9XtdqwOENXHtU6AcmCsgT4Mr792xg++bEqI=";
  };
  wlrootsSrc = fetchFromGitLab {
    domain = "gitlab.freedesktop.org";
    owner = "wlroots";
    repo = "wlroots";
    rev = "1712a7d27444d62f8da8eeedf0840b386a810e96";
    hash = "sha256-k7BFx1xvvsdCXNWX0XeZYwv8H/myk4p42i2Y6vjILqM=";
  };
  cl-waylandSrc = fetchFromGitHub {
    owner = "sdilts";
    repo = "cl-wayland";
    rev = "a92a5084b64102f538ab90212e99c7863e5338ae";
    hash = "sha256-7o/ScShTHdloFPCTiYP4xYfOlB9EfDTBWOI3wFqyjuQ=";
  };
  cl-xkbcommonSrc = fetchFromGitHub {
    owner = "sdilts";
    repo = "cl-xkbcommon";
    rev = "aa9513d93f42d7816f88dd1bd8bd21375e7d7512";
    hash = "sha256-OPScakcrqt3nTIGDT7ID0brLXhedo4jeTjxSf+Bc7Qs=";
  };

  # SBCL AND DEPS
  sbclCmd = "${sbcl}/bin/sbcl --script";
  sbclPackages = lispPackages_new.lispPackagesFor sbclCmd;

  cl-wayland = lispPackages_new.build-asdf-system {
    pname = "cl-wayland";
    version = "0.0.1";
    src = cl-waylandSrc;
    lisp = sbclCmd;
    lispLibs = lib.attrsets.attrVals ["cffi" "closer-mop" "xmls" "split-sequence"] sbclPackages;
    buildInputs = [
      # pkg-config
      wayland
    ];
    nativeBuildLibs = [wayland];
    nativeLibs = [wayland];
    systems = ["cl-wayland" "wayland-scanner"];
  };

  cl-xkbcommon = lispPackages_new.build-asdf-system {
    pname = "cl-xkbcommon";
    version = "0.0.1";
    src = cl-xkbcommonSrc;
    lisp = sbclCmd;
    lispLibs = lib.attrsets.attrVals ["cffi" "cffi-grovel"] sbclPackages;
    buildInputs = [pkg-config libxkbcommon];
    nativeBuildLibs = [libxkbcommon];
    nativeLibs = [libxkbcommon];
    systems = ["xkbcommon"];
  };

  cl-mahogany = lispPackages_new.build-asdf-system {
    inherit src version;
    pname = "cl-mahogany";
    lisp = sbclCmd;
    lispLibs =
      [cl-wayland cl-xkbcommon]
      ++ lib.attrsets.attrVals [
        "alexandria"
        "cffi"
        "cffi-grovel"
        "cl-ansi-text"
        "closer-mop"
        "iterate"
        "snakes"
        "terminfo"
      ]
      sbclPackages;
    systems = ["mahogany" "mahogany/executable"];
  };
  sbclWithMahogany = lispPackages_new.lispWithPackages sbclCmd (_: [cl-mahogany]);

  mahexeAsd = writeText "mahexe.asd" ''
    (asdf:defsystem "mahexe"
      :build-operation program-op
      :entry-point "mahogany::run-server"
      :build-pathname "build/mahogany"
      :depends-on (#:mahogany))
  '';

  libs = [
    "$out"
    # egl-wayland
    # eudev
    # ffmpeg_4
    libGL
    # libcap
    libdrm
    libinput
    # libpng
    libxkbcommon
    mesa
    pixman
    seatd
    vulkan-loader
    wayland
    wayland-protocols
    # xgboost
    # xorg.libX11
    # xorg.libxcb
    xorg.xcbutilerrors
    # xorg.xcbutilimage
    xorg.xcbutilrenderutil
    xorg.xcbutilwm
    xwayland
  ];
in
  stdenv.mkDerivation {
    inherit pname src version;

    nativeBuildInputs = [
      glslang
      hwdata
      meson
      ninja
      makeWrapper
      pkg-config
      sbclWithMahogany
      # wayland-scanner
    ];

    buildInputs = [
      # egl-wayland
      # eudev
      # ffmpeg_4
      libGL
      # libcap
      libdrm
      libinput
      # libpng
      libxkbcommon
      mesa
      pixman
      seatd
      vulkan-loader
      wayland
      wayland-protocols # no lib
      # xgboost
      # xorg.libX11
      # xorg.libxcb
      xorg.xcbutilerrors
      # xorg.xcbutilimage
      xorg.xcbutilrenderutil
      xorg.xcbutilwm
      xwayland
    ];

    postUnpack = ''
      cp -r ${wlrootsSrc}/* source/heart/subprojects/wlroots/
      cp -r ${cl-waylandSrc}/* source/dependencies/cl-wayland/
      cp -r ${cl-xkbcommonSrc}/* source/dependencies/cl-xkbcommon/
      chmod -R '+w' source/{heart/subprojects/wlroots,dependencies/{cl-wayland,cl-xkbcommon}}
    '';

    patches = [./mahogany.patch];

    dontConfigure = true;
    buildPhase = ''
      make $PWD/build/heart/lib64/libheart.so FORCE
      cat ${mahexeAsd} >mahexe.asd
      export CL_SOURCE_REGISTRY="$CL_SOURCE_REGISTRY:$PWD/"
      sbcl --eval "(require 'asdf)" --eval "(asdf:make \"mahexe\")"
    '';
    installPhase = ''
      mkdir -p $out/lib $out/bin
      mv build/mahogany $out/bin/
      mv build/lib/* $out/lib/
      wrapProgram $out/bin/mahogany --prefix LD_LIBRARY_PATH : "${lib.strings.makeLibraryPath libs}"
    '';
    dontStrip = true;
  }
