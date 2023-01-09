{
  outputs = {
    self,
    flake-utils,
    nixpkgs,
  }:
    {
      overlays = {
        csharp-ls = final: prev: {
          csharp-ls = final.callPackage ./nix/csharp-ls {};
        };
      };
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [self.overlays.csharp-ls];
      };
    in {
      devShells.default = pkgs.callPackage ./nix/shell.nix {};
      packages = {
        inherit (pkgs) csharp-ls;
      };
    });
}
