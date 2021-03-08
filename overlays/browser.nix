self: super: {
  browser = super.writeShellScriptBin "browser" ''
    browser=""
    while test -z "''${browser}"; do
        case "$(printf 'qutebrowser\nfirefox\nchromium\n' | '${self.dmenu}/bin/dmenu')" in
            qutebrowser) browser=qutebrowser ;;
            firefox) browser="${self.firefox}/bin/firefox" ;;
            chromium) browser="${self.ungoogledChromiumIncognito}/bin/chromium-incognito" ;;
        esac
    done
    "''${browser}" "$@"
  '';
}
