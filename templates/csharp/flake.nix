{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = {
    self,
    flake-utils,
    nixpkgs,
  }:
    {
      overlays = {
        csharp-ls = final: prev: {
          csharp-ls = prev.csharp-ls.overrideAttrs {useDotnetFromEnv = false;};
        };
        package = final: prev: {
          pacakge = final.callPackage ./nix/package {};
        };
      };
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [self.overlays.csharp-ls self.overlays.package];
      };
    in {
      apps = import ./nix/apps.nix pkgs;
      devShells.default = pkgs.callPackage ./nix/shell.nix {};
      packages = {inherit (pkgs) csharp-ls package;};
      checks = self.packages.${system};
    });
}
