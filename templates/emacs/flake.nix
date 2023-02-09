{
  description = "Emacs package";

  inputs = {
    eldev.flake = false;
    eldev.url = "github:doublep/eldev/1.3.1";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    eldev,
    emacs-overlay,
    nixpkgs,
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [self.overlays.eldev self.overlays.default];
    };

    inherit (builtins) attrNames elemAt foldl' head map match readDir readFile;
    inherit (builtins) stringLength tail;
    inherit (pkgs.lib.lists) filter;
    inherit (pkgs.lib.sources) sourceFilesBySuffices;
    inherit (pkgs.lib.strings) hasSuffix removeSuffix;
    parse = pkgs.callPackage "${emacs-overlay}/parse.nix" {};

    names = filter (hasSuffix ".el") (attrNames (readDir self));
    name = removeSuffix ".el" (foldl' (acc: elm:
      if (stringLength elm) < (stringLength acc)
      then elm
      else acc) (head names) (tail names));
    mainFile = readFile "${self}/${name}.el";

    version = elemAt (match ".*\n;; Version: ([^\n]+).*" mainFile) 0;
    url = elemAt (match ".*\n;; URL: ([^\n]+).*" mainFile) 0;
    deps = parse.parsePackagesFromPackageRequires mainFile;
  in {
    overlays = {
      default = final: prev: {
        emacsPackagesFor = emacs:
          (prev.emacsPackagesFor emacs).overrideScope' (
            efinal: eprev: {
              ${name} = efinal.melpaBuild {
                inherit version;
                pname = name;
                src = self;
                commit = self.rev;
                recipe = final.writeText "recipe" ''
                  (${name} :fetcher git :url "${url}")
                '';
                packageRequires = map (dep: efinal.${dep}) deps;
              };
            }
          );
      };

      eldev = final: prev: {
        eldev = final.stdenv.mkDerivation {
          name = "eldev";
          src = eldev;
          dontUnpack = true;
          dontPatch = true;
          dontConfigure = true;
          dontBuild = true;
          nativeBuildInputs = [final.emacs];
          installPhase = ''
            mkdir -p $out/bin
            cp $src/bin/eldev $out/bin/
          '';
        };
      };
    };

    devShells.${system}.default = pkgs.mkShell {
      inherit name;
      buildInputs = [pkgs.alejandra pkgs.eldev pkgs.statix];
      shellHook = ''
        export ELDEV_DIR=$PWD/.eldev
      '';
    };

    packages.${system} = {
      ${name} = (pkgs.emacsPackagesFor pkgs.emacs).${name};
    };

    checks.${system} =
      self.packages.${system}
      // {
        tests = pkgs.runCommand "run-tests" {} ''
          cp ${sourceFilesBySuffices ./. [".el"]}/* .
          loadfiles=""
          for file in *.el ; do
            loadfiles="$loadfiles -l $file"
          done
          ${pkgs.emacs}/bin/emacs -Q -module-assertions -batch \
            -L . $loadfiles -f ert-run-tests-batch-and-exit \
            && touch $out
        '';
      };
  };
}
