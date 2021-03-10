self: super: {
  browser = super.writeShellScriptBin "browser" ''
    browser=""
    while test -z "''${browser}"; do
        case "$(printf 'qutebrowser\nfirefox\nchromium\nytdl\nmpv\n' | '${self.dmenu}/bin/dmenu')" in
            qutebrowser) browser=qutebrowser ;;
            firefox) browser="${self.firefox}/bin/firefox" ;;
            chromium) browser="${self.ungoogledChromiumIncognito}/bin/chromium-incognito" ;;
            ytdl) browser="${self.scripts}/bin/ytdli" ;;
            mpv) browser="${self.scripts}/bin/mpvi" ;;
        esac
    done
    "''${browser}" "$@"
  '';
}
