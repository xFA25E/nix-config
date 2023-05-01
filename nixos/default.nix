inputs: let
  username = "val";
  mkNixos = module:
    inputs.nixpkgs.lib.nixosSystem {
      modules = [module];
      specialArgs = {inherit inputs username;};
      system = null;
    };
in {
  stribog = mkNixos ./stribog;
  perun = mkNixos ./perun.nix;
  khors = mkNixos ./khors;
}
