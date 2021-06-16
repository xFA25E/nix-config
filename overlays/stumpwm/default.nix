self: super: let
  deps = super.lib.strings.concatMapStringsSep " "
    (url: "\"${super.fetchzip url}/\"") (import ./deps.nix);

  my-deps = super.lib.strings.concatMapStringsSep " " (dep: "\"${dep}\"") [
    "dexador" "jsown" "trivia"
  ];

  defineDeps = super.writeText "define-deps.lisp" ''
    (dolist (dep '(${deps}))
      (push dep asdf:*central-registry*))
  '';

  loadMyDeps = super.writeText "load-my-deps.lisp" ''
    (asdf:load-system "cffi")
    (cffi:define-foreign-library libcrypto
     (:unix "${self.openssl.out}/lib/libcrypto.so"))
    (cffi:define-foreign-library libssl
     (:unix "${self.openssl.out}/lib/libssl.so"))
    (cffi:use-foreign-library libcrypto)
    (cffi:use-foreign-library libssl)

    (dolist (dep '(${my-deps}))
      (asdf:load-system dep))

    (defun stumpwm::data-dir () (uiop:xdg-cache-home "stumpwm/"))
  '';
  stumpwm = super.stdenv.mkDerivation rec {
    pname = "stumpwm";
    version = "20.11";
    src = super.fetchFromGitHub {
      owner = "stumpwm";
      repo = "stumpwm";
      rev = version;
      sha256 = "1ghs6ihvmb3bz4q4ys1d3h6rdi96xyiw7l2ip7jh54c25049aymf";
    };
    nativeBuildInputs = with self; [ sbcl autoconf texinfo makeWrapper pkgconfig ];
    buildInputs = with self; [ openssl.out ];
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
in {
  stumpwm = stumpwm;
  stumpwmDev = (import ./stumpwmDev.nix) self super;
}
