{
  alejandra,
  mkShell,
  statix,
}:
mkShell {
  buildInputs = [alejandra statix];
}
