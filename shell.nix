{
  alejandra,
  mkShell,
  rnix-lsp,
  shellcheck,
  statix,
}:
mkShell {
  packages = [alejandra shellcheck statix rnix-lsp];
}
