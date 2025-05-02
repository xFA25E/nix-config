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
  perun = mkNixos ./perun.nix;
  stribog = mkNixos ./stribog;
  svarog = mkNixos ./svarog;
}
