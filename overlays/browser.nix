self: super: {
  browser = super.writeShellScriptBin "browser" ''
    browser=""
    while test -z "''${browser}"; do
        case "$(printf 'chromium\nytdl\nmpv\n' | '${self.dmenu}/bin/dmenu')" in
            chromium) browser="${self.ungoogledChromiumIncognito}/bin/chromium-incognito" ;;
            ytdl) browser="${self.scripts}/bin/ytdli" ;;
            mpv) browser="${self.scripts}/bin/mpvi" ;;
        esac
    done
    "''${browser}" "$@"
  '';
}
