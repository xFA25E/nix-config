# TODOs
# prefix emacs packages with emacs-package- and use 'em in overlays
# extract base16 schemes from inputs based on base16-*-scheme
# extract to packages
# write update scrits for wallpapers, stumpwm, stardicts
# write modules and write nixos (like in Notifile)
{
  description = "xFA25E various nix configurations";

  inputs = {
    amded = {
      url = "github:ft/amded";
      flake = false;
    };
    emacs-amded.url = "github:xFA25E/amded";
    base16-summerfruit-scheme = {
      url = "github:cscorley/base16-summerfruit-scheme";
      flake = false;
    };
    base16-gruvbox-scheme = {
      url = "github:dawikur/base16-gruvbox-scheme";
      flake = false;
    };
    cyrillic-dvorak-im = {
      url = "github:xFA25E/cyrillic-dvorak-im";
      flake = false;
    };
    dired-tags = {
      url = "github:xFA25E/dired-tags";
      flake = false;
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    flymake-statix.url = "github:xFA25E/flymake-statix";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mpv-youtube-quality = {
      url = "github:jgreco/mpv-youtube-quality";
      flake = false;
    };
    nixos-options.url = "github:xFA25E/nixos-options";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    notmuch = {
      url = "git+https://git.notmuchmail.org/git/notmuch?ref=release";
      flake = false;
    };
    pueue.url = "github:xFA25E/pueue";
    rx-widget = {
      url = "github:xFA25E/rx-widget";
      flake = false;
    };
    rycee = {
      url = "gitlab:rycee/nur-expressions";
      flake = false;
    };
    sdcwoc = {
      url = "github:xFA25E/sdcwoc";
      flake = false;
    };
    skempo = {
      url = "github:xFA25E/skempo";
      flake = false;
    };
    stumpwm = {
      url = "github:stumpwm/stumpwm/22.05";
      flake = false;
    };
    unflac = {
      url = "git+https://git.sr.ht/~ft/unflac";
      flake = false;
    };
    xattr = {
      url = "github:xFA25E/xattr";
      flake = false;
    };
  };

  outputs = {
    self,
    amded,
    emacs-amded,
    base16-summerfruit-scheme,
    base16-gruvbox-scheme,
    cyrillic-dvorak-im,
    dired-tags,
    emacs-overlay,
    flake-utils,
    flymake-statix,
    home-manager,
    mpv-youtube-quality,
    nixos-options,
    nixpkgs,
    notmuch,
    pueue,
    rx-widget,
    rycee,
    sdcwoc,
    skempo,
    stumpwm,
    unflac,
    xattr,
  }: let
    system = "x86_64-linux";
    username = "val";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        emacs-amded.overlays.default
        emacs-overlay.overlay
        flymake-statix.overlays.default
        nixos-options.overlays.default
        pueue.overlays.default
        self.overlays.default
      ];
    };
  in {
    nixosConfigurations = {
      stribog = nixpkgs.lib.nixosSystem {
        inherit pkgs system;
        modules = [
          self.nixosModules.nix
          ./nixos/stribog.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.${username} = import ./home.nix;
            };
          }
        ];
        specialArgs = {inherit username;};
      };

      perun = nixpkgs.lib.nixosSystem {
        inherit pkgs system;
        modules = [self.nixosModules.nix ./nixos/perun.nix];
        specialArgs = {inherit username;};
      };
    };

    nixosModules.nix.config.nix = {
      registry = {
        nix-config.flake = self;
        nixpkgs.flake = nixpkgs;
      };
      settings.nix-path = ["nixpkgs=${nixpkgs}"];
    };

    overlays.default = final: prev: {
      amded = final.stdenv.mkDerivation {
        pname = "amded";
        version = "0.9";
        src = amded;
        nativeBuildInputs = [final.pkg-config];
        buildInputs = [final.taglib final.jsoncpp final.libb64 final.zlib];
        makeFlags = ["PREFIX=$(out)"];
        ADDTOCXXFLAGS = "-Wno-deprecated-declarations -DBUFFERSIZE=BUFSIZ";
      };

      base16Themes = let
        themes =
          final.runCommand "base16-themes" {
            srcs = [base16-summerfruit-scheme base16-gruvbox-scheme];
            nativeBuildInputs = [final.yq-go];
          } ''
            mkdir -p $out
            for src in $srcs; do
                for file in $src/*.yaml; do
                    yq e -o=json "$file" >"$out/$(basename "$file" .yaml).json"
                done
            done
          '';
      in
        themes
        // {
          theme = name: let
            inherit (builtins) fromJSON match readFile;
            inherit (final.lib.attrsets) filterAttrs;
            theme = fromJSON (readFile "${themes}/${name}.json");
          in
            filterAttrs (name: _: match "base.." name != null) theme;
        };

      brave = prev.brave.overrideAttrs (old: {
        postInstall = ''
          makeWrapper "$out/bin/brave" "$out/bin/brave-incognito" --add-flags -incognito
        '';
      });

      browser = final.writeShellScriptBin "browser" ''
        declare -A browsers=(
            ["firefox"]="${final.firefox}/bin/firefox"
            ["brave"]="${final.brave}/bin/brave-incognito"
            ["ytdl"]="${final.scripts.scripts.ytdli}/bin/ytdli"
            ["mpv"]="${final.scripts.scripts.mpvi}/bin/mpvi"
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

      discord = prev.discord.overrideAttrs (old: {
        src = final.fetchurl {
          url = "https://dl.discordapp.net/apps/linux/0.0.20/discord-0.0.20.tar.gz";
          hash = "sha256-3f7yuxigEF3e8qhCetCHKBtV4XUHsx/iYiaCCXjspYw=";
        };
      });

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

      emacsPackagesFor = emacs:
        (prev.emacsPackagesFor emacs).overrideScope' (efinal: eprev: let
          makePkg = args:
            efinal.melpaBuild ({
                commit = args.src.rev;
                recipe = final.writeText "recipe" ''
                  (${args.pname} :fetcher github :repo "xFA25E/${args.pname}")
                '';
              }
              // args);
        in {
          format-all = eprev.format-all.overrideAttrs ({patches ? [], ...}: {
            patches =
              patches
              ++ [
                (final.fetchpatch {
                  url = "https://github.com/lassik/emacs-format-all-the-code/pull/205.diff";
                  hash = "sha256-A3QHSk28kJ6uy1VGXNs0YXrNHt/pkcXKrGUeOM6Pxyk=";
                })
              ];
          });

          notmuch = eprev.notmuch.overrideAttrs (old: {
            inherit (final.notmuch) src version;
            commit = final.notmuch.src.rev;
          });

          pcmpl-args = eprev.pcmpl-args.overrideAttrs ({patches ? [], ...}: {
            patches =
              patches
              ++ [
                (final.fetchpatch {
                  url = "https://github.com/xFA25E/pcmpl-args.el/commit/fdc51e554160963fb2f4c9ce0041822e1515d1e3.diff";
                  sha256 = "0cscz3npna668ywp7302j5f3qrpk4xy5i8f347nv3rwrjhkmwh12";
                })
              ];
          });

          sly = eprev.sly.overrideAttrs ({
            patches ? [],
            prePatch ? "",
            ...
          }: {
            prePatch =
              prePatch
              + ''
                substituteInPlace lib/hyperspec.el --replace \
                  '"http://www.lispworks.com/reference/HyperSpec/"' \
                  '"file://${final.common-lisp-hyperspec}/HyperSpec/"'
              '';
            patches =
              patches
              ++ [
                (final.fetchpatch {
                  url = "https://github.com/xFA25E/sly/commit/4f95f882a7179170c09074c5c6986b407caa60f1.diff";
                  sha256 = "038c2cyw00r78zhimvvyv3dydbzbjx6d1p6683yyq0mjfd28c01y";
                })
              ];
          });

          transmission = eprev.transmission.overrideAttrs ({patches ? [], ...}: {
            patches =
              patches
              ++ [
                (final.fetchpatch {
                  url = "https://github.com/xFA25E/transmission/commit/a16a3516a84bb496da4b313f7185300c3def0f41.diff";
                  sha256 = "05zflff0ifmxjadgvszadw38v92kqsfsvq328596wjc47hzfdai1";
                })
              ];
          });

          tree-sitter = eprev.tree-sitter.overrideAttrs ({patches ? [], ...}: {
            patches =
              patches
              ++ [
                (final.fetchpatch {
                  url = "https://github.com/emacs-tree-sitter/elisp-tree-sitter/pull/231.diff";
                  hash = "sha256-3ZJF27x/sezNij0veMAy42gRxq48l88asuiJLWoshZo=";
                })
              ];
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
            packageRequires = [efinal.xattr];
          };

          rx-widget = makePkg {
            src = rx-widget;
            pname = "rx-widget";
            version = "0.0.1";
            packageRequires = [efinal.xr];
          };

          sdcwoc = makePkg {
            src = sdcwoc;
            pname = "sdcwoc";
            version = "0.0.2";
          };

          skempo = makePkg {
            src = skempo;
            pname = "skempo";
            version = "0.2.2";
          };

          xattr = makePkg {
            src = xattr;
            pname = "xattr";
            version = "0.0.3";
            recipe = final.writeText "recipe" ''
              (xattr :fetcher github :repo "xFA25E/axttr" :files ("xattr-core.so" "xattr.el" "xattr-map.el"))
            '';
            EMACS_SRC = "${final.emacs}/share/emacs/${final.emacs.version}/src";
            nativeBuildInputs = [final.gnulib];
            preBuild = "make";
          };
        });

      materia-theme = final.callPackage "${rycee}/pkgs/materia-theme" {};

      mpv-youtube-quality = final.runCommand "mpv-youtube-quality" {} ''
        mkdir -p $out
        ln -s ${mpv-youtube-quality}/youtube-quality.* $out
      '';

      notmuch = (prev.notmuch.override {withEmacs = false;}).overrideAttrs (old: {
        version = "0.36";
        src = notmuch;
      });

      scripts = import ./scripts/default.nix final;

      stardicts =
        final.runCommand "stardict-dictionaries" {
          srcs = let
            inherit (final.lib.strings) removePrefix removeSuffix;
            fns = [baseNameOf (removePrefix "stardict-") (removeSuffix ".tar.bz2")];
            makeName = dic: {name = final.lib.trivial.pipe dic.url fns;};
          in
            map (dic: final.fetchzip (dic // makeName dic)) (import ./stardicts.nix);
        } ''
          mkdir -p "$out/share/stardict/dic"
          for src in $srcs; do
              ln -s "$src" "$out/share/stardict/dic/$(stripHash $src)"
          done
        '';

      stumpwm = import ./stumpwm {
        src = stumpwm;
        pkgs = final;
        slynk = false;
      };

      unflac = final.buildGoModule {
        name = "unflac";
        src = unflac;
        vendorSha256 = "sha256-R5Sa7pYRg79tkZ0jsupjvJVZ6D5jqN1syPz/YR5wF8g=";
        proxyVendor = true;
      };

      wallpapers = let
        wallpapers = map final.fetchurl (import ./wallpapers.nix);
      in
        final.runCommand "wallpapers" {
          srcs = builtins.tail wallpapers;
          pepe = builtins.head wallpapers;
          nativeBuildInputs = [final.imagemagick];
        } ''
          mkdir -p $out
          for src in $srcs; do ln -s $src $out/$(stripHash $src); done
          convert $pepe -resize 1366x -crop 1366x768+0+200 $out/pepefishing.jpg
        '';
    };

    packages.${system} = let
      inherit (pkgs.lib.attrsets) filterAttrs isDerivation;
      overlayPkgs = self.overlays.default pkgs pkgs;
    in
      filterAttrs (k: isDerivation) overlayPkgs;

    apps.${system} = {
      default = {
        type = "app";
        program = "${pkgs.scripts.scripts.preparehd}/bin/preparehd";
      };
      switch = {
        type = "app";
        program = toString (pkgs.writeShellScript "switch" ''
          sudo -A nixos-rebuild switch --print-build-logs --flake .
        '');
      };
    };

    templates = import ./templates/default.nix;

    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [pkgs.alejandra pkgs.statix];
    };
  };
}
