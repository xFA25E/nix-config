self: super: let
  libb64 = self.libb64.overrideAttrs (old: {
    prePatch = if old ? "prePatch" then old.prePatch else "" + ''
      substituteInPlace include/b64/encode.h --replace BUFFERSIZE BUFSIZ
    '';
  });
in {
  amded = super.stdenv.mkDerivation {
    name = "amded";
    version = "0.8";
    src = super.fetchFromGitHub {
      owner = "ft";
      repo = "amded";
      rev = "8394ae4c761c960c4b72fc2ebf2694bb9769f2b4";
      sha256 = "1w632qxrxamndjdki6vpc2xbl6136fi13w6p3yc9r3hw4wl8bhz5";
    };

    nativeBuildInputs = [ self.pkg-config ];
    buildInputs = [ self.taglib self.jsoncpp libb64 self.zlib ];

    prePatch = ''
      substituteInPlace file-spec.cpp \
        --replace \
        "return fh->save(save_tags, false, 4, false);" \
        "return fh->save(save_tags, TagLib::MPEG::File::StripTags::StripNone, TagLib::ID3v2::v4, TagLib::MPEG::File::DuplicateTags::DoNotDuplicate);"
    '';

    makeFlags = [ "PREFIX=$(out)" ];
  };
}
