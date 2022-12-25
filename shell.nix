{
  alejandra,
  mkShell,
  rnix-lsp,
  shellcheck,
  statix,
}:
mkShell {
  buildInputs = [alejandra shellcheck statix rnix-lsp];
}
