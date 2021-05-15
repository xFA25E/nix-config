self: super: {
  browser = super.writeShellScriptBin "browser" ''
    browser=""
    while test -z "''${browser}"; do
        case "$(printf 'qutebrowser\nchromium\nytdl\nmpv\n' | '${self.dmenu}/bin/dmenu')" in
            qutebrowser) browser=${self.qutebrowser}/bin/qutebrowser ;;
            chromium) browser="${self.ungoogledChromiumIncognito}/bin/chromium-incognito" ;;
            ytdl) browser="${self.scripts}/bin/ytdli" ;;
            mpv) browser="${self.scripts}/bin/mpvi" ;;
        esac
    done
    "''${browser}" "$@"
  '';
}
