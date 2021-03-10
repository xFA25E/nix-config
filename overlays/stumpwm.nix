self: super: let
  alexandria = fetchGit {
    url = "https://gitlab.common-lisp.net/alexandria/alexandria.git";
    rev = "f35e232ceb2ada8d10e7fdf27ccac07f781eea0e";
  };
  cl-ppcre = super.fetchFromGitHub {
    owner = "edicl";
    repo = "cl-ppcre";
    rev = "v2.1.1";
    sha256 = "0dwvr29diqzcg5n6jvbk2rnd90i05l7n828hhw99khmqd0kz7xsi";
  };
  clx = super.fetchFromGitHub {
    owner = "sharplispers";
    repo = "clx";
    rev = "0.7.5";
    sha256 = "1vi67z9hpj5rr4xcmfbfwzmlcc0ah7hzhrmfid6lqdkva238v2wf";
  };
  cl-utilities = super.fetchzip {
    url = "http://common-lisp.net/project/cl-utilities/cl-utilities-latest.tar.gz";
    sha256 = "1dmbkdr8xm2jw5yx1makqbf1ypqbm0hpkd7zyknxv3cblvz0a87w";
  };
  xml-emitter = super.fetchFromGitHub {
    owner = "VitoVan";
    repo = "xml-emitter";
    rev = "1a93a5ab084a10f3b527db3043bd0ba5868404bf";
    sha256 = "1w9yx8gc4imimvjqkhq8yzpg3kjrp2y37rjix5c1lnz4s7bxvhk9";
  };
  global-vars = super.fetchFromGitHub {
    owner = "lmj";
    repo = "global-vars";
    rev = "c749f32c9b606a1457daa47d59630708ac0c266e";
    sha256 = "06m3xc8l3pgsapl8fvsi9wf6y46zs75cp9zn7zh6dc65v4s5wz3d";
  };
  trivial-features = super.fetchFromGitHub {
    owner = "trivial-features";
    repo = "trivial-features";
    rev = "v0.9";
    sha256 = "1iczrsl561fz9f71dzals16749fccznm4jn8nmxnqas1qk7b331k";
  };
  trivial-garbage = super.fetchFromGitHub {
    owner = "trivial-garbage/";
    repo = "trivial-garbage";
    rev = "v0.21";
    sha256 = "0122jicfg7pca1wxw8zak1n92h5friqy60988ns0ysksj3fphw9n";
  };
  bordeaux-threads = super.fetchFromGitHub {
    owner = "sionescu";
    repo = "bordeaux-threads";
    rev = "v0.8.8";
    sha256 = "19i443fz3488v1pbbr9x24y8h8vlyhny9vj6c9jk5prm702awrp6";
  };
  jsown = super.fetchFromGitHub {
    owner = "madnificent";
    repo = "jsown";
    rev = "744c4407bef58dfa876d9da0b5c0205d869e7977";
    sha256 = "0gadvmf1d9bq35s61z76psrsnzwwk12svi66jigf491hv48wigw7";
  };
  slynk = super.runCommand "slynk" {
    src = super.fetchFromGitHub {
      owner = "joaotavora";
      repo = "sly";
      rev = "1.0.43";
      sha256 = "11yclc8i6gpy26m1yj6bid6da22639zpil1qzj87m5gfvxiv4zg6";
    };
  } ''
    cp -r "$src/slynk" "$out"
  '';
  babel = super.fetchFromGitHub {
    owner = "cl-babel";
    repo = "babel";
    rev = "f892d0587c7f3a1e6c0899425921b48008c29ee3";
    sha256 = "04frn19mngvsh8bh7fb1rfjm8mqk8bgzx5c43dg7z02nfsxkqqak";
  };
  trivial-gray-streams = super.fetchFromGitHub {
    owner = "trivial-gray-streams";
    repo = "trivial-gray-streams";
    rev = "2b3823edbc78a450db4891fd2b566ca0316a7876";
    sha256 = "1hipqwwd5ylskybd173rvlsk7ds4w4nq1cmh9952ivm6dgh7pwzn";
  };
  closure-common = super.fetchFromGitHub {
    owner = "sharplispers";
    repo = "closure-common";
    rev = "e3c5f5f454b72b01b89115e581c3c52a7e201e5c";
    sha256 = "0k5r2qxn122pxi301ijir3nayi9sg4d7yiy276l36qmzwhp4mg5n";
  };
  puri = fetchGit {
    url = "https://gitlab.common-lisp.net/clpm/puri.git";
    rev = "4bbab89d9ccbb26346899d1f496c97604fec567b";
  };
  cxml = super.fetchFromGitHub {
    owner = "sharplispers";
    repo = "cxml";
    rev = "8701da08ba4aac30891b8d2005edb018c1d3d796";
    sha256 = "18fls3bx7vmnxfa6qara8fxp316d8kb3izar0kysvqg6l0a45a51";
  };
  cl-xmlspam = super.fetchzip {
    url = "https://common-lisp.net/project/cl-xmlspam/cl-xmlspam.tgz";
    sha256 = "03jw57889b60nsqgb13vrf5q1g2fasah7qv7knjlx2w4mc1ci7ks";
  };
  flexi-streams = super.fetchFromGitHub {
    owner = "edicl";
    repo = "flexi-streams";
    rev = "v1.0.19";
    sha256 = "0v7lh4nrldzczd4mwylvmxfdxk7wfsli24iv1axd6mkb833llr70";
  };
  cffi = super.fetchFromGitHub {
    owner = "cffi";
    repo = "cffi";
    rev = "v0.23.0";
    sha256 = "03s98imc5niwnpj3hhrafl7dmxq45g74h96sm68976k7ahi3vl5b";
  };
  split-sequence = super.fetchFromGitHub {
    owner = "sharplispers";
    repo = "split-sequence";
    rev = "v2.0.0";
    sha256 = "0jcpnx21hkfwqj5fvp7kc6pn1qcz9hk7g2s5x8h0349x1j2irln0";
  };
  idna = super.fetchFromGitHub {
    owner = "antifuchs";
    repo = "idna";
    rev = "0.2.2";
    sha256 = "00nbr3mffxhlq14gg9d16pa6691s4qh35inyw76v906s77khm5a2";
  };
  swap-bytes = super.fetchFromGitHub {
    owner = "sionescu";
    repo = "swap-bytes";
    rev = "v1.2";
    sha256 = "1hw1v1lw26rifyznpnj1csphha9jgzwpiic16ni3pvs6hcsni9rz";
  };
  iolib = super.fetchFromGitHub {
    owner = "sionescu";
    repo = "iolib";
    rev = "v0.8.3";
    sha256 = "0pa86bf3jrysnmhasbc0lm6cid9xzril4jsg02g3gziav1xw5x2m";
  };
  ironclad = super.fetchFromGitHub {
    owner = "sharplispers";
    repo = "ironclad";
    rev = "v0.54";
    sha256 = "07g0wpvfqq2yk23prs890d4qvbnr3xd6w8ssd88g89xdg483wpvk";
  };
  ieee-floats = super.fetchFromGitHub {
    owner = "marijnh";
    repo = "ieee-floats";
    rev = "566b51a005e81ff618554b9b2f0b795d3b29398d";
    sha256 = "1xyj49j9x3lc84cv3dhbf9ja34ywjk1c46dklx425fxw9mkwm83m";
  };
  dbus = super.fetchFromGitHub {
    owner = "death";
    repo = "dbus";
    rev = "f4d1a99cfb38ded33f4fb58bb5522815f530b3c3";
    sha256 = "1gw5414q7r6yi2bm1wk2fhqnvhxzc5c6812z3qh67c9dyizcjy2a";
  };
  proc-parse = super.fetchFromGitHub {
    owner = "fukamachi";
    repo = "proc-parse";
    rev = "3afe2b76f42f481f44a0a495256f7abeb69cef27";
    sha256 = "07vbj26bfq4ywlcmamsqyac29rsdsa8lamjqx1ycla1bcvgmi4w2";
  };
  xsubseq = super.fetchFromGitHub {
    owner = "fukamachi";
    repo = "xsubseq";
    rev = "5ce430b3da5cda3a73b9cf5cee4df2843034422b";
    sha256 = "1xz79q0p2mclf3sqjiwf6izdpb6xrsr350bv4mlmdlm6rg5r99px";
  };
  smart-buffer = super.fetchFromGitHub {
    owner = "fukamachi";
    repo = "smart-buffer";
    rev = "09b9a9a0b3abaa37abe9a730f5aac2643dca4e62";
    sha256 = "0qz1zzxx0wm5ff7gpgsq550a59p0qj594zfmm2rglj97dahj54l7";
  };
  fast-http = super.fetchFromGitHub {
    owner = "fukamachi";
    repo = "fast-http";
    rev = "502a37715dcb8544cc8528b78143a942de662c5a";
    sha256 = "0al2g7g219jjljsf7b23pbilpgacxy5as5gs2nqf76b5qni396mi";
  };
  quri = super.fetchFromGitHub {
    owner = "fukamachi";
    repo = "quri";
    rev = "f90194e7fdd745f3e94c1829970c0e256c18ff5d";
    sha256 = "0ply9zzc192sb4cbm7w93s0cwih7yfr5n9zrwafibq9cs019niv4";
  };
  static-vectors = super.fetchFromGitHub {
    owner = "sionescu";
    repo = "static-vectors";
    rev = "v1.8.6";
    sha256 = "01hwxzhyjkhsd3949g70120g7msw01byf0ia0pbj319q1a3cq7j9";
  };
  fast-io = super.fetchFromGitHub {
    owner = "rpav";
    repo = "fast-io";
    rev = "ab5942c1b54bf6b4b66beec519ebb081fb39387f";
    sha256 = "131cqlf84md6kgw2slrpgmksz2j3w1rx4a0cwfrkd8kdvwbh16rd";
  };
  chunga = super.fetchFromGitHub {
    owner = "edicl";
    repo = "chunga";
    rev = "v1.1.7";
    sha256 = "0jzn3nyb3f22gm983rfk99smqs3mhb9ivjmasvhq9qla5cl9pyhd";
  };
  local-time = super.fetchFromGitHub {
    owner = "dlowe-net";
    repo = "local-time";
    rev = "a177eb911c0e8116e2bfceb79049265a884b701b";
    sha256 = "0wld28xx20k0ysgg6akic5lg4vkjd0iyhv86m388xfrv8xh87wii";
  };
  cl-cookie = super.fetchFromGitHub {
    owner = "fukamachi";
    repo = "cl-cookie";
    rev = "9c077eb652dd108a30726154f46b9a7d482f5474";
    sha256 = "1kphfjbh9hzjc95ad7mpfsb0x7d8f7xznlaskr8awymspbmck8cz";
  };
  trivial-mimes = super.fetchFromGitHub {
    owner = "Shinmera";
    repo = "trivial-mimes";
    rev = "a741fc2f567a4f86b853fd4677d75e62c03e51d9";
    sha256 = "00kcm17q5plpzdj1qwg83ldhxksilgpcdkf3m9azxcdr968xs9di";
  };
  chipz = super.fetchFromGitHub {
    owner = "sharplispers";
    repo = "chipz";
    rev = "0f6c9ea7ac903d41288a79f4cf38ffec21ba7cd2";
    sha256 = "1l6cvks7slp5a0wag5vhbhn8972lfxamci59jd1ai4icv1vv1jsk";
  };
  cl-base64 = fetchGit {
    url = "https://gitlab.common-lisp.net/clpm/cl-base64.git";
    rev = "fc62a5342445d4ec1dd44e95f7dc513473a8c89a";
  };
  cl-reexport = super.fetchFromGitHub {
    owner = "takagi";
    repo = "cl-reexport";
    rev = "03ad7a0e7307e510b3b25a622d3c5f8a077879b4";
    sha256 = "02la6z3ickhmh2m87ymm2ijh9nkn7l6slskj99l8a1rhps394qqc";
  };
  usocket = super.fetchFromGitHub {
    owner = "usocket";
    repo = "usocket";
    rev = "v0.8.3";
    sha256 = "0x746wr2324l6bn7skqzgkzcbj5kd0zp2ck0c8rldrw0rzabg826";
  };
  cl-plus-ssl = super.fetchFromGitHub {
    owner = "cl-plus-ssl";
    repo = "cl-plus-ssl";
    rev = "9b74f743ecc664faafcb1e27060d21bbd8ec326f";
    sha256 = "18d9l420n63530m8h9wkx9bgjgz18w9lc0aqqixnrdx8zv72q0w6";
  };
  dexador = super.fetchFromGitHub {
    owner = "fukamachi";
    repo = "dexador";
    rev = "8a668a91fdcaaf3754479a48cf8c443d7a9f2b31";
    sha256 = "07ny3v2ivihjffrys1vli57fy6brvdf43ry8xa6d76hm4pyadzhm";
  };
  notify = super.runCommand "notify" {
    src = super.fetchFromGitHub {
      owner = "stumpwm";
      repo = "stumpwm-contrib";
      rev = "a7dc1c663d04e6c73a4772c8a6ad56a34381096a";
      sha256 = "09akdaaya7lga5lzbq1aj1filsyjwvflghkidpmr0nk0jz5xx1g7";
    };
  } ''
    cp -r "$src/util/notify" "$out"
  '';

  asdf-deps = super.lib.strings.concatMapStringsSep " " (d: "\"${d}/\"") [
    alexandria babel bordeaux-threads cffi chipz chunga cl-base64 cl-cookie
    cl-plus-ssl cl-ppcre cl-reexport cl-utilities cl-xmlspam closure-common clx
    cxml dbus dexador fast-http fast-io flexi-streams global-vars idna
    ieee-floats iolib ironclad jsown local-time notify proc-parse puri quri
    smart-buffer split-sequence slynk static-vectors swap-bytes trivial-features
    trivial-garbage trivial-gray-streams trivial-mimes usocket xml-emitter
    xsubseq
  ];

  define-asdf-deps = super.writeText "define-asdf-deps.lisp" ''
    (dolist (dep '(${asdf-deps}))
      (push dep asdf:*central-registry*))
  '';
  load-asdf-deps = super.writeText "load-asdf-deps.lisp" ''
    (asdf:load-system "cffi")
    (pushnew #p"${self.libfixposix}/lib/" cffi:*foreign-library-directories* :test #'equal)
    (cffi:define-foreign-library libcrypto
     (:unix "${self.openssl.out}/lib/libcrypto.so"))
    (cffi:define-foreign-library libssl
     (:unix "${self.openssl.out}/lib/libssl.so"))
    (cffi:use-foreign-library libcrypto)
    (cffi:use-foreign-library libssl)
    (dolist (dep '("xml-emitter" "bordeaux-threads" "jsown" "slynk" "dbus" "dexador"
                   "notify"))
      (asdf:load-system dep))
  '';
in {
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
    buildInputs = with self; [ libfixposix openssl.out ];
    configurePhase = ''
        ./autogen.sh
        ./configure --prefix=$out --with-module-dir=$out/share/stumpwm/modules
      '';
    preBuild = ''
        substituteInPlace load-stumpwm.lisp --replace "(require 'asdf)" "(require 'asdf) (load \"${define-asdf-deps}\")"
        echo "(load \"${load-asdf-deps}\")" >> load-stumpwm.lisp
        mkdir tmptmp
        export HOME=$PWD/tmptmp
      '';
    dontStrip = true;
  };
}
