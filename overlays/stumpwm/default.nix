self: super: let
  inherit (super) fetchFromGitHub fetchzip writeText;
  inherit (super.lib.strings) makeSearchPath concatMapStringsSep;
  inherit (super.lib.trivial) importJSON;
  inherit (super.stdenv) mkDerivation;

  slynk = fetchFromGitHub {
    name = "slynk";
    owner = "joaotavora";
    repo = "sly";
    rev = "eb67be9698794ba66a09f46b7cfffab742863a91";
    sha256 = "11yclc8i6gpy26m1yj6bid6da22639zpil1qzj87m5gfvxiv4zg6";
  };

  dependencies = (map fetchzip (importJSON ./dependency-urls.json)) ++ [ "${slynk}/slynk" ];
  to-load = (importJSON ./dependencies.json) ++ [
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

  load-my-deps = writeText "load-my-deps.lisp" ''
    (in-package :cl-user)
    (require "asdf")

    (asdf:load-system "cffi")
    (cffi:define-foreign-library libcrypto
     (:unix "${self.openssl.out}/lib/libcrypto.so"))
    (cffi:define-foreign-library libssl
     (:unix "${self.openssl.out}/lib/libssl.so"))
    (cffi:use-foreign-library libcrypto)
    (cffi:use-foreign-library libssl)

    (mapc #'asdf:load-system (list ${concatMapStringsSep " " (item: "\"${item}\"") to-load}))
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
    buildInputs = with self; [ openssl.out ];
    CL_SOURCE_REGISTRY = "${makeSearchPath "" dependencies}:";
    configurePhase = ''
      ./autogen.sh
      ./configure --prefix=$out --with-module-dir=$out/share/stumpwm/modules
    '';
    preBuild = ''
      makeFlagsArray+=(sbcl_BUILDOPTS="--load ${load-my-deps} --load ./make-image.lisp")
      export ASDF_OUTPUT_TRANSLATIONS="(:output-translations :ignore-inherited-configuration (t \"$out/share/stumpwm/common-lisp/\"))"
    '';
    dontStrip = true;
  };
}
