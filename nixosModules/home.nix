{
  inputs,
  username,
  ...
}: {
  imports = [inputs.home-manager.nixosModules.home-manager];

  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${username} = inputs.self.homeModules.base;
  };

  nixpkgs = {
    overlays = [
      inputs.nur.overlays.default
      inputs.emacs-overlay.overlays.default
      inputs.self.overlays.default
    ];
  };
}
