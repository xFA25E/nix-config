{
  pkgs,
  inputs,
  ...
}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./../xdg/emacs/init.el;
      package = pkgs.emacs;
      extraEmacsPackages = epkgs:
        [epkgs.treesit-grammars.with-all-grammars]
        ++ map (flake: inputs."epkg-${flake}".packages.${pkgs.stdenv.hostPlatform.system}.default) [
          "amded"
          "cyrillic-dvorak-im"
          "dired-atool-transient"
          "dired-tags"
          "pueue"
          "rx-widget"
          "sdcwoc"
          "tempo-extra"
        ];

      override = efinal: eprev: {
        debase = let
          rev = "5a728897d332229f7c7cc5beb21f806ee41bc12c";
          url = "https://codeberg.org/xFA25E/debase.git";
        in
          efinal.melpaBuild {
            version = "0.8";
            pname = "debase";
            src = pkgs.fetchgit {
              inherit rev url;
              hash = "sha256-29vT1E4SfRVuz8BY4D7h4zTPUy/hEAFSdAuWAJ/z3ys=";
            };
            commit = rev;
            recipe = pkgs.writeText "recipe" ''(debase :fetcher git :url "${url}")'';
          };

        discomfort = let
          rev = "637d40cae658e69f3d65cd0af4f5077da61d7f71";
          url = "https://codeberg.org/xFA25E/discomfort.git";
        in
          efinal.melpaBuild {
            version = "0.1";
            pname = "discomfort";
            src = pkgs.fetchgit {
              inherit rev url;
              hash = "sha256-/h50P2gji1WAtRIalRUzuJfOpa7qRRsFuEMmAACq5lo=";
            };
            commit = rev;
            recipe = pkgs.writeText "recipe" ''(discomfort :fetcher git :url "${url}")'';
            packageRequires = [efinal.debase];
          };

        flymake-collection = eprev.flymake-collection.overrideAttrs ({patches ? [], ...}: {
          patches =
            patches
            ++ map pkgs.fetchpatch [
              {
                url = "https://github.com/xFA25E/flymake-collection/commit/00319c59b78f054fd39806ed58c5df0bdecf0be2.diff";
                hash = "sha256-ZXDAkrTAVmBuK/7c4LbGAm1+YzuvDcSRxgGvVs2Reok=";
              }
            ];
        });

        notmuch = eprev.notmuch.overrideAttrs {inherit (pkgs.notmuch) src version;};

        org-roam = eprev.org-roam.overrideAttrs ({patches ? [], ...}: {
          patches =
            patches
            ++ map pkgs.fetchpatch [
              {
                url = "https://github.com/xFA25E/org-roam/commit/1e9620d21a952105c7baf6d7a15fba0ec654aafa.diff";
                hash = "sha256-KvOxBCpweDyQvCYOUFH8tu9VG/WlSck5vlQXsDr2O9I=";
              }
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
              substituteInPlace lib/hyperspec.el --replace-fail \
                '"https://www.lispworks.com/reference/HyperSpec/"' \
                '"file://${pkgs.cl-hyperspec}/HyperSpec/"'
            '';
          # patches =
          #   patches
          #   ++ map pkgs.fetchpatch [
          #     {
          #       url = "https://github.com/joaotavora/sly/pull/441.diff";
          #       hash = "sha256-+rwVmrN4A4GY8oI3GiuMXq8JUyd0gT1oqdMA6NtTpnU=";
          #     }
          #   ];
        });

        transmission = eprev.transmission.overrideAttrs ({patches ? [], ...}: {
          patches =
            patches
            ++ map pkgs.fetchpatch [
              {
                url = "https://github.com/xFA25E/transmission/commit/a16a3516a84bb496da4b313f7185300c3def0f41.diff";
                hash = "sha256-IarmPjyESW5SQWLgrZ3GU6SNBm/q6/2akr26CJyj7hc=";
              }
            ];
        });
      };
    };
  };
}
