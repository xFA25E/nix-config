{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) listToAttrs;
  inherit (config.colorScheme) colors;
  inherit (lib.attrsets) nameValuePair;
  inherit (lib.strings) escapeShellArg;
in {
  xdg.configFile =
    (listToAttrs (map (name:
      nameValuePair "emacs/${name}" {
        onChange = ''
          ${config.programs.emacs.finalPackage}/bin/emacs -Q --batch --eval \
            '(byte-compile-file (pop command-line-args-left))' \
            ${escapeShellArg "${config.xdg.configHome}/emacs/${name}"}
        '';
        source = "${./emacs}/${name}";
      }) [
      "init.el"
      "early-init.el"
      "skempo/emacs-lisp.el"
      "skempo/lisp.el"
      "skempo/js.el"
      "skempo/nix.el"
    ]))
    // {
      "emacs/abbrev_defs".source = ./emacs/abbrev_defs;
      "emacs/custom.el".source = ./emacs/custom.el;

      "mpv/scripts/youtube-quality.lua".source = "${pkgs.mpv-youtube-quality}/youtube-quality.lua";
      "mpv/script-opts/youtube-quality.conf".source = "${pkgs.mpv-youtube-quality}/youtube-quality.conf";

      "npm/npmrc".text = lib.generators.toKeyValue {} {
        prefix = "${config.xdg.dataHome}/npm";
        cache = "${config.xdg.cacheHome}/npm";
        init-module = "${config.xdg.configHome}/npm/config/npm-init.js";
        tmp = "\${XDG_RUNTIME_DIR}/npm";
      };

      "stumpwm/config".text = with colors; ''
        (swm-config:init)
        (stumpwm:set-fg-color "#${base04}")
        (stumpwm:set-bg-color "#${base00}")
        (stumpwm:set-border-color "#${base03}")
        (stumpwm:set-focus-color "#${base04}")
        (stumpwm:set-unfocus-color "#${base00}")
        (setf stumpwm:*colors* '("#${base00}" "#${base08}" "#${base0B}" "#${base0A}" "#${base0D}" "#${base0E}" "#${base0C}" "#${base05}"))
        (mapc #'stumpwm:update-color-map stumpwm:*screen-list*)
      '';

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
        };
    };
}
