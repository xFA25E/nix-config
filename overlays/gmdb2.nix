{
  autoconf,
  automake,
  gettext,
  gtk3,
  itstool,
  libtool,
  mdbtools,
  pkg-config,
  src,
  stdenv,
  yelp-tools,
  which,
  wrapGAppsHook3,
}:
stdenv.mkDerivation {
  inherit src;
  name = "gmdb2";
  nativeBuildInputs = [
    autoconf
    automake
    gettext
    gtk3
    itstool
    libtool
    mdbtools
    pkg-config
    yelp-tools
    which
    wrapGAppsHook3
  ];
  preConfigure = "autoreconf -f -i";
}
