self: super: {
  browser = super.writeShellScriptBin "browser" ''
    browser=""
    # when
    if test -z "''${browser}"; then
        case "$(printf 'firefox\nbrave\nytdl\nmpv\n' | '${self.dmenu}/bin/dmenu')" in
            firefox) browser="${self.firefox}/bin/firefox" ;;
            brave) browser="${self.braveIncognito}/bin/brave-incognito" ;;
            ytdl) browser="${self.scripts}/bin/ytdli" ;;
            mpv) browser="${self.scripts}/bin/mpvi" ;;
        esac
    fi
    "''${browser}" "$@"
  '';
}
