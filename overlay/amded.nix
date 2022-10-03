{
  jsoncpp,
  libb64,
  pkg-config,
  src,
  stdenv,
  taglib,
  zlib,
}:
stdenv.mkDerivation {
  inherit src;
  pname = "amded";
  version = builtins.head (builtins.match
    ".*\n#define VERSION \"([.0-9]+)\".*"
    (builtins.readFile "${src}/amded.h"));
  nativeBuildInputs = [pkg-config];
  buildInputs = [jsoncpp libb64 taglib zlib];
  makeFlags = ["PREFIX=$(out)"];
  ADDTOCXXFLAGS = "-Wno-deprecated-declarations -DBUFFERSIZE=BUFSIZ";
}
