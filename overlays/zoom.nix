self: super: {
  zoom = let
    pulseaudioSupport = true;
    version = "5.5.7938.0228";
    srcs = {
      x86_64-linux = super.fetchurl {
        url = "https://zoom.us/client/${version}/zoom_x86_64.pkg.tar.xz";
        sha256 = "KM8o2tgIn0lecOM4gKdTOdk/zsohlFqtNX+ca/S6FGY=";
      };
    };

    libs = super.lib.makeLibraryPath (with self; [
      # $ LD_LIBRARY_PATH=$NIX_LD_LIBRARY_PATH:$PWD ldd zoom | grep 'not found'
      alsaLib
      atk
      cairo
      dbus
      libGL
      fontconfig
      freetype
      gtk3
      gdk-pixbuf
      glib
      pango
      stdenv.cc.cc
      wayland
      xorg.libX11
      xorg.libxcb
      xorg.libXcomposite
      xorg.libXext
      libxkbcommon
      xorg.libXrender
      zlib
      xorg.xcbutilimage
      xorg.xcbutilkeysyms
      xorg.libXfixes
      xorg.libXtst
    ] ++ super.lib.optional (pulseaudioSupport) self.libpulseaudio);

  in super.stdenv.mkDerivation rec {
    pname = "zoom";
    inherit version;
    src = srcs.${super.stdenv.hostPlatform.system};

    dontUnpack = true;

    nativeBuildInputs = [
      super.makeWrapper
    ];

    installPhase = ''
      runHook preInstall
      mkdir $out
      tar -C $out -xf ${src}
      mv $out/usr/* $out/
      runHook postInstall
    '';

    postFixup = ''
      # Desktop File
      substituteInPlace $out/share/applications/Zoom.desktop \
          --replace "Exec=/usr/bin/zoom" "Exec=$out/bin/zoom"

      for i in zopen zoom ZoomLauncher; do
        patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $out/opt/zoom/$i
      done

      # ZoomLauncher sets LD_LIBRARY_PATH before execing zoom
      wrapProgram $out/opt/zoom/zoom \
        --prefix LD_LIBRARY_PATH ":" ${libs}

      rm $out/bin/zoom
      # Zoom expects "zopen" executable (needed for web login) to be present in CWD. Or does it expect
      # everybody runs Zoom only after cd to Zoom package directory? Anyway, :facepalm:
      makeWrapper $out/opt/zoom/ZoomLauncher $out/bin/zoom \
        --run "cd $out/opt/zoom" \
        --prefix PATH : ${super.lib.makeBinPath (with self; [ coreutils glib.dev pciutils procps qt5.qttools.dev utillinux ])} \
        --prefix LD_LIBRARY_PATH ":" ${libs}

      # Backwards compatiblity: we used to call it zoom-us
      ln -s $out/bin/{zoom,zoom-us}
    '';

    # already done
    dontPatchELF = true;

    passthru.updateScript = ./update.sh;

    meta = {
      homepage = "https://zoom.us/";
      description = "zoom.us video conferencing application";
      license = super.lib.licenses.unfree;
      platforms = builtins.attrNames srcs;
      maintainers = with super.lib.maintainers; [ danbst tadfisher doronbehar ];
    };
  };
}
