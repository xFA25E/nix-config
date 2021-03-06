self: super: {
  ungoogledChromiumIncognito = super.writeShellScriptBin "chromium-incognito" ''
    exec "${self.ungoogled-chromium}/bin/chromium" -incognito "$@"
  '';
}
