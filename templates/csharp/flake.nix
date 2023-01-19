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
        libSkiaSharp = final: prev: {
          libSkiaSharp = final.callPackage ./nix/libSkiaSharp.nix {};
        };
      };
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [self.overlays.csharp-ls self.overlays.libSkiaSharp];
      };
    in {
      devShells.default = pkgs.callPackage ./nix/shell.nix {};
      packages = {inherit (pkgs) csharp-ls libSkiaSharp;};
    });
}
