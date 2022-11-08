{
  alejandra,
  mkShell,
  shellcheck,
  statix,
}:
mkShell {
  buildInputs = [alejandra shellcheck statix];
}
