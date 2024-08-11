{
  agenix,
  alejandra,
  mkShell,
  rnix-lsp,
  shellcheck,
  statix,
}:
mkShell {
  packages = [agenix alejandra shellcheck statix];
  shellHook = "export RULES=$PWD/nixos/khors/secrets/secrets.nix";
}
