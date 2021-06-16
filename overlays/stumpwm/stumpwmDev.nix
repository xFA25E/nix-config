self: super: let
  slynk = super.fetchFromGitHub {
    owner = "joaotavora";
    repo = "sly";
    rev = "5966d68727898fa6130fb6bb02208f70aa8d5ce3";
    sha256 = "00yk9g0gi4gsa99n2gsq41mkwgvmih52mngmk4g8ajzxsv0pbwq0";
  };

  deps = (super.lib.strings.concatMapStringsSep " "
    (url: "\"${super.fetchzip url}/\"")
    (import ./deps.nix)) + " \"${slynk}/slynk/\"";

  my-deps = super.lib.strings.concatMapStringsSep " " (dep: "\"${dep}\"") [
    "dexador" "jsown" "trivia" "slynk"
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

    (defun stumpwm-user::slynk-start () ()
      (let ((quicklisp-init (uiop:xdg-cache-home "quicklisp" "setup.lisp")))
        (if (probe-file quicklisp-init)
            (progn (load quicklisp-init)
                   (slynk:create-server :port 4096))
            (stumpwm::message "Cannot find quicklisp file!"))))
  '';

in super.stdenv.mkDerivation rec {
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
      asdfdir=$(mktemp -d /tmp/.common-lisp.XXXXX)
      export HOME=$asdfdir
      substituteInPlace load-stumpwm.lisp --replace "(require 'asdf)" "(require 'asdf) (load \"$defineDeps\")"
      echo "(load \"$loadMyDeps\")" >>load-stumpwm.lisp
    '';
  postInstall = ''
      mkdir -p $out/share
      mv $asdfdir $out/share/
      mv $out/bin/stumpwm $out/bin/.stumpwm-wrapped
      cat >$out/bin/stumpwm <<EOF
      #!/bin/sh
      cp -r $out/share/$(basename $asdfdir) /tmp/
      chmod -R +w $asdfdir
      $out/bin/.stumpwm-wrapped "\$@"
      rm -rf $asdfdir
      EOF
      chmod +x $out/bin/stumpwm
    '';
  dontStrip = true;
  inherit loadMyDeps;
  inherit defineDeps;
}
