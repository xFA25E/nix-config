{
  agenix,
  alejandra,
  mkShell,
  # rnix-lsp,
  shellcheck,
  statix,
}:
mkShell {
  packages = [agenix alejandra shellcheck statix];
  shellHook = "export RULES=$PWD/nixosConfigurations/khors/secrets/secrets.nix";
}
