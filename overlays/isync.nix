self: super: let
  cyrus-sasl-xoauth2-src = super.fetchFromGitHub {
    owner = "moriyoshi";
    repo = "cyrus-sasl-xoauth2";
    rev = "36aabca54fd65c8fa7a707cb4936751599967904";
    sha256 = "02bjzydw7drskkn9v1wwc7f3i17r324lycv3gnsd129xq6w8fn9s";
  };
  cyrus_sasl_with_xoauth2 = super.cyrus_sasl.overrideAttrs (oldAttrs: {
    postInstall =  ''
      echo INSTALLING XOUATH2
      mkdir -p cyrus-sasl-xoauth2
      cp -t cyrus-sasl-xoauth2 ${cyrus-sasl-xoauth2-src}/*
      cd cyrus-sasl-xoauth2
      export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -isystem $dev/include"
      ./autogen.sh
      ./configure --with-cyrus-sasl=$out
      make
      make install
      cd ..
    '';
  });
in {
  # inherit cyrus_sasl_with_xoauth2;
  # isync = super.isync.override { cyrus_sasl = cyrus_sasl_with_xoauth2; };
}
