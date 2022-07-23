{
  inputs.eldev = { url = "github:doublep/eldev/1.1.3"; flake = false; };
  outputs = { self, nixpkgs, eldev }: let
    name = "foo-bar";
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.eldev ]; };

    inherit (builtins) elemAt match readFile;
    mainFile = readFile "${self}/${name}.el";
    version = elemAt (match ".*\n;; Version: ([^\n]+).*" mainFile) 0;
    url = elemAt (match ".*\n;; URL: ([^\n]+).*" mainFile) 0;
  in {

    overlays = {
      default = self.overlays.${name};

      ${name} = final: prev: {
        emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope' (
          efinal: eprev: {
            ${name} = efinal.melpaBuild {
              inherit version;
              pname = name;
              src = self;
              commit = self.rev;
              recipe = final.writeText "recipe" ''
                (${name} :fetcher git :url "${url}")
              '';
              packageRequires = with efinal; [];
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
          nativeBuildInputs = [ final.emacs ];
          installPhase = ''
            cp -r $src/* .
            mkdir -p $out/bin
            ELDEV_DIR=$out/share/eldev ./install.sh $out/bin
          '';
        };
      };

    };

    devShells.${system}.default = pkgs.mkShell {
      inherit name;
      buildInputs = [ pkgs.eldev ];
      ELDEV_DIR = "${builtins.toString ./.}/.eldev";
      shellHook = ''
        mkdir -p .eldev
        ${pkgs.xorg.lndir}/bin/lndir -silent ${pkgs.eldev}/share/eldev .eldev
      '';
    };

  };
}
