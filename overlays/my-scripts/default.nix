self: super: {
  myScripts = super.stdenv.mkDerivation {
    name = "my-scripts";
    src = ./scripts;
    installPhase = ''
      install -D -t "$out/bin" "$src/"*
    '';
  };
}
