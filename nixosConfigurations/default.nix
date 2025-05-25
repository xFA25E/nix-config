inputs: system: pkgs: let
  username = "val";
  mkNixos = module:
    inputs.nixpkgs.lib.nixosSystem {
      inherit system pkgs;
      modules = [module];
      specialArgs = {inherit inputs username;};
    };
in {
  khors = mkNixos ./khors;
  stribog = mkNixos ./stribog.nix;
  svarog = mkNixos ./svarog.nix;
  veles = mkNixos ./veles.nix;
}
