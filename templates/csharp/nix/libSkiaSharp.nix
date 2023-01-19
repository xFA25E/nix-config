{
  expat,
  fetchFromGitHub,
  fetchgit,
  fontconfig,
  gn,
  icu,
  libX11,
  libglvnd,
  libjpeg,
  libpng,
  libwebp,
  mesa,
  ninja,
  python2,
  runCommand,
  stdenv,
  zlib,
}: let
  version = "2.88.3";

  skiaSharp = fetchFromGitHub {
    owner = "mono";
    repo = "SkiaSharp";
    rev = "v${version}";
    hash = "sha256-HmGN/iODzf9n2n0No9gWkQjGfDW9jw9hTODEI90acCc=";
  };

  versionScript = "${skiaSharp}/native/linux/libSkiaSharp/libSkiaSharp.map";
  soname = builtins.readFile (runCommand "skia-soname" {} ''
    awk '$1 == "libSkiaSharp" && $2 == "soname" {printf "%s", $3}' <${skiaSharp}/VERSIONS.txt >$out
  '');

  angle2 = fetchgit {
    url = "https://chromium.googlesource.com/angle/angle.git";
    rev = "47b3db22be33213eea4ad58f2453ee1088324ceb";
    sha256 = "sha256-ZF5wDOqh3cRfQGwOMay//4aWh9dBWk/cLmUsx+Ab2vw=";
  };

  piex = fetchgit {
    url = "https://android.googlesource.com/platform/external/piex.git";
    rev = "bb217acdca1cc0c16b704669dd6f91a1b509c406";
    sha256 = "05ipmag6k55jmidbyvg5mkqm69zfw03gfkqhi9jnjlmlbg31y412";
  };
in
  stdenv.mkDerivation {
    inherit version;

    pname = "libSkiaSharp";

    src = fetchFromGitHub {
      owner = "mono";
      repo = "skia";
      rev = "v${version}";
      sha256 = "sha256-FqVWnFN+dBBp6iDQxDCkohShbCz58QXMlXruyF8UYM4=";
    };

    nativeBuildInputs = [gn ninja python2];

    buildInputs = [
      expat
      fontconfig
      icu
      libX11
      libglvnd
      libjpeg
      libpng
      libwebp
      mesa
      zlib
    ];

    preConfigure = ''
      mkdir -p third_party/externals
      ln -s ${angle2} third_party/externals/angle2
      ln -s ${piex} third_party/externals/piex
    '';

    configurePhase = ''
      runHook preConfigure
      gn gen out/linux/x64 --args='
        extra_asmflags=[]
        extra_cflags=[ "-DSKIA_C_DLL", "-DHAVE_SYSCALL_GETRANDOM", "-DXML_DEV_URANDOM" ]
        extra_ldflags=[ "-static-libstdc++", "-static-libgcc", "-Wl,--version-script=${versionScript}" ]
        is_official_build=true
        linux_soname_version="${soname}"
        skia_enable_gpu=true
        skia_enable_skottie=true
        skia_enable_tools=false
        skia_use_dng_sdk=false
        skia_use_icu=false
        skia_use_piex=true
        skia_use_sfntly=false
        skia_use_vulkan=false
        target_cpu="x64"
        target_os="linux"
      '
      runHook postConfigure
    '';

    buildPhase = ''
      runHook preBuild
      ninja -C out/linux/x64 SkiaSharp
      runHook postBuild
    '';

    installPhase = ''
      mkdir -p $out/lib
      cp out/linux/x64/libSkiaSharp.so.${soname}  $out/lib
      ln -s $out/lib/libSkiaSharp.so.${soname} $out/lib/libSkiaSharp.so
    '';
  }
