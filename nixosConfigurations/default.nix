inputs: let
  username = "val";
  mkNixos = module:
    inputs.nixpkgs.lib.nixosSystem {
      modules = [module];
      specialArgs = {inherit inputs username;};
      system = null;
    };
in {
  khors = mkNixos ./khors;
  stribog = mkNixos ./stribog.nix;
  svarog = mkNixos ./svarog.nix;
  veles = mkNixos ./veles.nix;
}
