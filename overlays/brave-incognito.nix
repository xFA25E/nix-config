self: super: {
  braveIncognito = super.writeShellScriptBin "brave-incognito" ''
    exec "${self.brave}/bin/brave" -incognito "$@"
  '';
}
