self: super: let
  contrib = super.fetchFromGitHub {
    owner = "stumpwm";
    repo = "stumpwm-contrib";
    rev = "a7dc1c663d04e6c73a4772c8a6ad56a34381096a";
    sha256 = "09akdaaya7lga5lzbq1aj1filsyjwvflghkidpmr0nk0jz5xx1g7";
  };

  deps = super.lib.strings.concatMapStringsSep " "
    (url: "\"${super.fetchzip url}/\"") (import ./deps.nix) + " \"${contrib}/util/notify/\"";

  my-deps = super.lib.strings.concatMapStringsSep " " (dep: "\"${dep}\"") [
    "dexador" "jsown" "xml-emitter" "bordeaux-threads" "dbus" "notify"
  ];

  defineDeps = super.writeText "define-deps.lisp" ''
    (dolist (dep '(${deps}))
      (push dep asdf:*central-registry*))
  '';

  loadMyDeps = super.writeText "load-my-deps.lisp" ''
    (asdf:load-system "cffi")
    (pushnew #p"${self.libfixposix}/lib/" cffi:*foreign-library-directories* :test #'equal)
    (cffi:define-foreign-library libcrypto
     (:unix "${self.openssl.out}/lib/libcrypto.so"))
    (cffi:define-foreign-library libssl
     (:unix "${self.openssl.out}/lib/libssl.so"))
    (cffi:use-foreign-library libcrypto)
    (cffi:use-foreign-library libssl)

    (dolist (dep '(${my-deps}))
      (asdf:load-system dep))
  '';

in {
  stumpwm =  super.stdenv.mkDerivation rec {
    pname = "stumpwm";
    version = "20.11";
    src = super.fetchFromGitHub {
      owner = "stumpwm";
      repo = "stumpwm";
      rev = version;
      sha256 = "1ghs6ihvmb3bz4q4ys1d3h6rdi96xyiw7l2ip7jh54c25049aymf";
    };
    nativeBuildInputs = with self; [ sbcl autoconf texinfo makeWrapper pkgconfig ];
    buildInputs = with self; [ libfixposix openssl.out ];
    configurePhase = ''
      ./autogen.sh
      ./configure --prefix=$out --with-module-dir=$out/share/stumpwm/modules
    '';
    preBuild = ''
      mkdir -p $PWD/tmp
      export HOME=$PWD/tmp
      substituteInPlace load-stumpwm.lisp --replace "(require 'asdf)" "(require 'asdf) (load \"$defineDeps\")"
      echo "(load \"$loadMyDeps\")" >>load-stumpwm.lisp
    '';
    dontStrip = true;
    inherit loadMyDeps;
    inherit defineDeps;
  };
}
