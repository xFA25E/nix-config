{
  description = "xFA25E various nix configurations";

  inputs = {
    amded = { url = "github:ft/amded"; flake = false; };
    base16-summerfruit-scheme = { url = "github:cscorley/base16-summerfruit-scheme"; flake = false; };
    base16-gruvbox-scheme = { url = "github:dawikur/base16-gruvbox-scheme"; flake = false; };
    cyrillic-dvorak-im = { url = "github:xFA25E/cyrillic-dvorak-im"; flake = false; };
    dired-tags = { url = "github:xFA25E/dired-tags"; flake = false; };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = { url = "github:nix-community/home-manager/release-22.05"; inputs.nixpkgs.follows = "nixpkgs"; };
    mpv-youtube-quality = { url = "github:jgreco/mpv-youtube-quality"; flake = false; };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    notmuch = { url = "git+https://git.notmuchmail.org/git/notmuch?ref=release"; flake = false; };
    pueue = { url = "github:xFA25E/pueue"; flake = false; };
    rx-widget = { url = "github:xFA25E/rx-widget"; flake = false; };
    sdcwoc = { url = "github:xFA25E/sdcwoc"; flake = false; };
    shell-pwd = { url = "github:xFA25E/shell-pwd"; flake = false; };
    skempo = { url = "github:xFA25E/skempo"; flake = false; };
    stumpwm = { url = "github:stumpwm/stumpwm/22.05"; flake = false; };
    taggit = { url = "github:xFA25E/taggit"; flake = false; };
    unflac = { url = "git+https://git.sr.ht/~ft/unflac"; flake = false; };
    xattr = { url = "github:xFA25E/xattr"; flake = false; };
  };

  outputs = {
    self
    , amded
    , base16-summerfruit-scheme
    , base16-gruvbox-scheme
    , cyrillic-dvorak-im
    , dired-tags
    , emacs-overlay
    , flake-utils
    , home-manager
    , mpv-youtube-quality
    , nixpkgs
    , notmuch
    , pueue
    , rx-widget
    , sdcwoc
    , shell-pwd
    , skempo
    , stumpwm
    , taggit
    , unflac
    , xattr
  }: let
    system = "x86_64-linux";
    username = "val";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [ emacs-overlay.overlay self.overlays.default ];
    };
  in {
    homeConfigurations."${username}" = home-manager.lib.homeManagerConfiguration {
      inherit system username pkgs;
      configuration = import ./home.nix;
      extraModules = [ self.nixosModules.nix ];
      homeDirectory = "/home/${username}";
      stateVersion = "22.05";
    };

    nixosConfigurations = {
      stribog = nixpkgs.lib.nixosSystem {
        inherit pkgs system;
        modules = [
          ./nixos/stribog.nix
          self.nixosModules.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              sharedModules = [ self.nixosModules.nix ];
              useGlobalPkgs = true;
              useUserPackages = true;
              users.${username} = import ./home.nix;
            };
          }
        ];
        specialArgs = { inherit username; };
      };

      perun = nixpkgs.lib.nixosSystem {
        inherit pkgs system;
        modules = [ ./nixos/perun.nix self.nixosModules.nix ];
        specialArgs = { inherit username; };
      };
    };

    nixosModules.nix = { ... }: {
      config.nix = {
        package = pkgs.nix;
        registry."nix-config" = {
          from = { id = "nix-config"; type = "indirect"; };
          to = { owner = "xFA25E"; repo = "nix-config"; type = "github"; };
        };
        settings = {
          bash-prompt-suffix = "dev ";
          experimental-features = [ "nix-command" "flakes" ];
          keep-derivations = true;
          keep-outputs = true;
          max-jobs = "auto";
        };
      };
    };

    overlays.default = final: prev: {

      amded = final.stdenv.mkDerivation {
        pname = "amded";
        version = "0.9";
        src = amded;
        nativeBuildInputs = [ final.pkg-config ];
        buildInputs = [ final.taglib final.jsoncpp final.libb64 final.zlib ];
        makeFlags = [ "PREFIX=$(out)" ];
        ADDTOCXXFLAGS = "-Wno-deprecated-declarations -DBUFFERSIZE=BUFSIZ";
      };

      base16Themes = let
        themes = pkgs.runCommand "base16-themes" {
          srcs = [ base16-summerfruit-scheme base16-gruvbox-scheme ];
          nativeBuildInputs = [ final.yq-go ];
        } ''
          mkdir -p $out
          for src in $srcs; do
              for file in $src/*.yaml; do
                  yq e -o=json "$file" >"$out/$(basename "$file" .yaml).json"
              done
          done
        '';
      in (themes // {
        theme = name: let
          json = builtins.readFile "${themes}/${name}.json";
          theme = builtins.fromJSON json;
          addHash = name: value: if builtins.match "base.." name == null then value else "#" + value;
        in builtins.mapAttrs addHash theme;
      });

      brave = prev.brave.overrideAttrs (old: {
        postInstall = ''
          makeWrapper "$out/bin/brave" "$out/bin/brave-incognito" --add-flags -incognito
        '';
      });

      browser = final.writeShellScriptBin "browser" ''
        declare -A browsers=(
            ["firefox"]="${final.firefox}/bin/firefox"
            ["brave"]="${final.brave}/bin/brave-incognito"
            ["ytdl"]="${final.scripts}/bin/ytdli"
            ["mpv"]="${final.scripts}/bin/mpvi"
        )
        choice=$(printf '%s\n' "''${!browsers[@]}" | "${final.dmenu}/bin/dmenu" || printf firefox)
        exec "''${browsers[$choice]}" "$@"
      '';

      common-lisp-hyperspec = final.fetchzip {
        name = "common-lisp-hyperspec";
        url = "http://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz";
        sha256 = "1zsi35245m5sfb862ibzy0pzlph48wvlggnqanymhgqkpa1v20ak";
        stripRoot = false;
      };

      dmenu = prev.dmenu.override {
        patches = [
          (final.fetchpatch {
            url = "https://tools.suckless.org/dmenu/patches/case-insensitive/dmenu-caseinsensitive-5.0.diff";
            sha256 = "sha256-XqFEBRu+aHaAXrNn+WXnkIuC/vAHDIb/im2UhjaPYC0=";
          })
          (final.fetchpatch {
            url = "https://tools.suckless.org/dmenu/patches/xresources-alt/dmenu-xresources-alt-5.0.diff";
            sha256 = "sha256-znx7FoXnddHWsZaB2liTNhM/szktnlgecDwVhax6waA=";
          })
        ];
      };

      emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope' (efinal: eprev: let
        makePkg = args: efinal.melpaBuild ({
          commit = args.src.rev;
          recipe = final.writeText "recipe" ''
            (${args.pname} :fetcher github :repo "xFA25E/${args.pname}")
          '';
        } // args);
      in {

        notmuch = eprev.notmuch.overrideAttrs (old: {
          inherit (final.notmuch) src version;
          commit = final.notmuch.src.rev;
        });

        pcmpl-args = eprev.pcmpl-args.overrideAttrs ({ patches ? [], ...}: {
          patches = patches ++ [ (final.fetchpatch {
            url = "https://github.com/xFA25E/pcmpl-args.el/commit/fdc51e554160963fb2f4c9ce0041822e1515d1e3.diff";
            sha256 = "0cscz3npna668ywp7302j5f3qrpk4xy5i8f347nv3rwrjhkmwh12";
          }) ];
        });

        sly = eprev.sly.overrideAttrs ({ patches ? [], prePatch ? "", ...}: {
          prePatch = prePatch + ''
            substituteInPlace lib/hyperspec.el --replace \
              '"http://www.lispworks.com/reference/HyperSpec/"' \
              '"file://${final.common-lisp-hyperspec}/HyperSpec/"'
          '';
          patches = patches ++ [ (final.fetchpatch {
            url = "https://github.com/xFA25E/sly/commit/4f95f882a7179170c09074c5c6986b407caa60f1.diff";
            sha256 = "038c2cyw00r78zhimvvyv3dydbzbjx6d1p6683yyq0mjfd28c01y";
          }) ];
        });

        transmission = eprev.transmission.overrideAttrs ({ patches ? [], ...}: {
          patches = patches ++ [ (final.fetchpatch {
            url = "https://github.com/xFA25E/transmission/commit/a16a3516a84bb496da4b313f7185300c3def0f41.diff";
            sha256 = "05zflff0ifmxjadgvszadw38v92kqsfsvq328596wjc47hzfdai1";
          }) ];
        });

        cyrillic-dvorak-im = makePkg {
          src = cyrillic-dvorak-im;
          pname = "cyrillic-dvorak-im";
          version = "0.1.0";
        };

        dired-tags = makePkg {
          src = dired-tags;
          pname = "dired-tags";
          version = "0.0.2";
          packageRequires = [ efinal.xattr ];
        };

        pueue = makePkg {
          src = pueue;
          pname = "pueue";
          version = "1.0.4";
          packageRequires = [ efinal.transient ];
        };

        rx-widget = makePkg {
          src = rx-widget;
          pname = "rx-widget";
          version = "0.0.1";
          packageRequires = [ efinal.xr ];
        };

        sdcwoc = makePkg {
          src = sdcwoc;
          pname = "sdcwoc";
          version = "0.0.2";
        };

        shell-pwd = makePkg {
          src = shell-pwd;
          pname = "shell-pwd";
          version = "0.2";
        };

        skempo = makePkg {
          src = skempo;
          pname = "skempo";
          version = "0.2.2";
        };

        taggit = makePkg {
          src = taggit;
          pname = "taggit";
          version = "0.0.1";
        };

        xattr = makePkg {
          src = xattr;
          pname = "xattr";
          version = "0.0.3";
          recipe = final.writeText "recipe" ''
            (xattr :fetcher github :repo "xFA25E/axttr" :files ("xattr-core.so" "xattr.el" "xattr-map.el"))
          '';
          EMACS_SRC = "${final.emacs}/share/emacs/${final.emacs.version}/src";
          nativeBuildInputs = [ final.gnulib ];
          preBuild = "make";
        };

      });

      mpv-youtube-quality = final.runCommand "mpv-youtube-quality" {} ''
        mkdir -p $out
        ln -s ${mpv-youtube-quality}/youtube-quality.* $out
      '';

      notmuch = (prev.notmuch.override { withEmacs = false; }).overrideAttrs (old: {
        version = "0.36";
        src = notmuch;
      });

      scripts = let paths = pkgs.lib.strings.makeBinPath (with pkgs; [
        "$out" dmenu ffmpeg file findutils gawk gnugrep gnused jq libnotify mpv
        mtpfs mu pass pueue qrencode sxiv unixtools.column utillinux xclip
        youtube-dl
      ]); in final.stdenv.mkDerivation {
        name = "scripts";
        src = ./scripts;
        dontUnpack = true;
        dontPatch = true;
        dontConfigure = true;
        dontBuild = true;
        nativeBuildInputs = [ final.makeWrapper ];
        installPhase = ''
          install -D -t $out/bin $src/*
          for s in $out/bin/*; do
              wrapProgram $s --prefix PATH : "${paths}";
          done
          for y in ${final.ytdl}/bin/*; do
              makeWrapper $y $out/bin/$(basename $y) --add-flags "--exec '$out/bin/filename_put_duration {}'"
          done
        '';
      };

      stardicts = final.runCommand "stardict-dictionaries" {
        srcs = let
          inherit (final.lib.strings) removePrefix removeSuffix;
          fns = [ baseNameOf (removePrefix "stardict-") (removeSuffix ".tar.bz2") ];
          makeName = dic: { name = final.lib.trivial.pipe dic.url fns; };
        in map (dic: final.fetchzip (dic // makeName dic)) (import ./stardicts.nix);
      } ''
        mkdir -p "$out/share/stardict/dic"
        for src in $srcs; do
            ln -s "$src" "$out/share/stardict/dic/$(stripHash $src)"
        done
      '';

      unflac = final.buildGoModule {
        name = "unflac";
        src = unflac;
        vendorSha256 = "sha256-R5Sa7pYRg79tkZ0jsupjvJVZ6D5jqN1syPz/YR5wF8g=";
        proxyVendor = true;
      };

      stumpwm = import ./stumpwm { src = stumpwm; pkgs = final; slynk = false; };

      wallpapers = let
        wallpapers = map final.fetchurl (import ./wallpapers.nix);
      in final.runCommand "wallpapers" {
        srcs = builtins.tail wallpapers;
        pepe = builtins.head wallpapers;
        nativeBuildInputs = [ final.imagemagick ];
      } ''
        mkdir -p $out
        for src in $srcs; do ln -s $src $out/$(stripHash $src); done
        convert $pepe -resize 1366x -crop 1366x768+0+200 $out/pepefishing.jpg
      '';

      youtube-dl = prev.youtube-dl.overrideAttrs ({ patches, ... }: {
        patches = patches ++ [
          (final.fetchpatch {
            url = "https://github.com/ytdl-org/youtube-dl/commit/a0068bd6bec16008bda7a39caecccbf84881c603.diff";
            sha256 = "1b03fz3jx719zzsyrzy93gg4lyhmdjia73kkv9dfp9i3rsg88axm";
          })
          (final.fetchpatch {
            url = "https://github.com/xFA25E/youtube-dl/commit/2e9fa3dffc43c32fc9776d651def58ee36c70f5f.diff";
            sha256 = "1a4cpwr88vqmraykhf5rfif7wlj99ks2jba09qidfsngcipx05xw";
          })
        ];
      });

      ytdl = let
        inherit (pkgs.lib.strings) escapeShellArg;
        y = "${pkgs.youtube-dl}/bin/youtube-dl";
        pre = "\${YTDL_DIR:-\${XDG_VIDEOS_DIR:-\${HOME}/Videos}/youtube}/";
        suf = "%(channel)s - %(upload_date)s - %(title)s.%(ext)s";
        vfmt = "${pre}${suf}";
        pfmt = "${pre}%(playlist_uploader)s - %(playlist)s - %(playlist_index)s - ${suf}";
        vflags = "--output \"${vfmt}\"";
        pflags = "--output \"${pfmt}\"";
        aflags = "--format bestaudio/best --extract-audio";
        wrap = n: f: "makeWrapper ${y} $out/bin/${n} --add-flags ${escapeShellArg f}";
      in final.runCommand "ytdl" { nativeBuildInputs = [ final.makeWrapper ]; } ''
        mkdir -p $out
        ${wrap "ytdl" vflags}
        ${wrap "ytdla" (vflags + " " + aflags)}
        ${wrap "ytdlp" pflags}
        ${wrap "ytdlpa" (pflags + " " + aflags)}
      '';

    };

    packages.${system} = let
      inherit (pkgs.lib.attrsets) filterAttrs isDerivation;
      overlayPkgs = self.overlays.default pkgs pkgs;
    in filterAttrs (k: v: isDerivation v) overlayPkgs;

    apps.${system}.default = {
      type = "app";
      program = let
        c = builtins.readFile ./apps/partitionEncryptFormatMount;
        s = pkgs.writeShellScript "partitionEncryptFormatMount" c;
      in "${s}";
    };

  };
}
