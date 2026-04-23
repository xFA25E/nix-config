{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) listToAttrs;
  inherit (config.colorScheme) palette;
  inherit (lib.attrsets) nameValuePair;
  inherit (lib.strings) escapeShellArg;
in {
  xdg.configFile = {
    "emacs/abbrev_defs".source = ./emacs/abbrev_defs;
    "emacs/early-init.el".source = ./emacs/early-init.el;
    "emacs/init.el".source = ./emacs/init.el;

    "ledger/ledgerrc".text = ''
      --start-of-week 1
    '';

    "npm/npmrc".text = lib.generators.toKeyValue {} {
      prefix = "${config.xdg.dataHome}/npm";
      cache = "${config.xdg.cacheHome}/npm";
      init-module = "${config.xdg.configHome}/npm/config/npm-init.js";
      tmp = "\${XDG_RUNTIME_DIR}/npm";
    };

    "stumpwm/config".text = "(swm-config:init)";

    "wgetrc".text = "hsts-file=${config.xdg.cacheHome}/wget-hsts";

    "yt-dlp/config".text =
      lib.generators.toKeyValue {
        mkKeyValue = lib.generators.mkKeyValueDefault {} " ";
      } {
        "--add-metadata" = "";
        "--compat-options" = "no-live-chat";
        "--continue" = "";
        "--embed-subs" = "";
        "--format" = "'(bestvideo+bestaudio/best)[height<=?768][width<=?1366]/(bestvideo+bestaudio/best)[height<=?1080][width<=?1920]/bestvideo+bestaudio/best'";
        "--ignore-errors" = "";
        "--no-playlist" = "";
        "--output" = "'%(channel,uploader)s - %(upload_date)s - %(title)s.%(ext)s'";
        "--cookies-from-browser" = "firefox";
      };
  };
}
