self: super: {
  browser = super.writeShellScriptBin "browser" ''
    browser=""
    while test -z "''${browser}"; do
        case "$(printf 'brave\nytdl\nmpv\n' | '${self.dmenu}/bin/dmenu')" in
            brave) browser="${self.braveIncognito}/bin/brave-incognito" ;;
            ytdl) browser="${self.scripts}/bin/ytdli" ;;
            mpv) browser="${self.scripts}/bin/mpvi" ;;
        esac
    done
    "''${browser}" "$@"
  '';
}
