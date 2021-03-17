self: super: let
  contrib = super.fetchFromGitHub {
    owner = "stumpwm";
    repo = "stumpwm-contrib";
    rev = "a7dc1c663d04e6c73a4772c8a6ad56a34381096a";
    sha256 = "09akdaaya7lga5lzbq1aj1filsyjwvflghkidpmr0nk0jz5xx1g7";
  };

  quicklisp-quickstart = super.fetchurl {
    url = "https://beta.quicklisp.org/quicklisp.lisp";
    sha256 = "05rcxg7rrkp925s155p0rk848jp2jxrjcm3q0hbn8wg0xcm5qyja";
  };

  quickload-deps = super.writeText "quickload-deps.lisp" ''
    (ql:quickload "alexandria")
    (ql:quickload "cl-ppcre")
    (ql:quickload "clx")
    (ql:quickload "cffi")
    (pushnew #p"${self.libfixposix}/lib/" cffi:*foreign-library-directories* :test #'equal)
    (cffi:define-foreign-library libcrypto
     (:unix "${self.openssl.out}/lib/libcrypto.so"))
    (cffi:define-foreign-library libssl
     (:unix "${self.openssl.out}/lib/libssl.so"))
    (cffi:use-foreign-library libcrypto)
    (cffi:use-foreign-library libssl)
    (ql:quickload "xml-emitter")
    (ql:quickload "bordeaux-threads")
    (ql:quickload "jsown")
    (ql:quickload "slynk")
    (ql:quickload "dbus")
    (ql:quickload "dexador")

    (push "${self.emacsPackages.sly-asdf}/share/emacs/site-lisp/elpa/sly-asdf-20200306.433/" asdf:*central-registry*)
    (asdf:load-system "slynk-asdf")
  '';

  load-stumpwm-modules = super.writeText "load-asdf-deps.lisp" ''
    (push "${contrib}/util/notify/" asdf:*central-registry*)
    (asdf:load-system "notify")
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
      mkdir -p $out/share/quicklisp
      export HOME=$out/share
      echo | sbcl --load "${quicklisp-quickstart}" --eval "(quicklisp-quickstart:install :path #p\"$out/share/quicklisp/\")" --eval "(ql:add-to-init-file)"
      echo '(load "${quickload-deps}")' >>$HOME/.sbclrc
      echo '(load "${load-stumpwm-modules}")' >>load-stumpwm.lisp
    '';
    dontStrip = true;
  };
}
