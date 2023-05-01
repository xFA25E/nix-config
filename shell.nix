{
  agenix,
  alejandra,
  mkShell,
  rnix-lsp,
  shellcheck,
  statix,
}:
mkShell {
  packages = [agenix alejandra shellcheck statix rnix-lsp];
  shellHook = "export RULES=$PWD/nixos/khors/secrets/secrets.nix";
}
