{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) listToAttrs;
  inherit (lib) generators;
  inherit (lib.attrsets) nameValuePair;
in {
  imports = [./packages.nix];

  home = {
    extraOutputsToInstall = ["man" "doc" "info" "devdoc"];

    file.".stalonetrayrc".text =
      generators.toKeyValue {
        mkKeyValue = generators.mkKeyValueDefault {} " ";
      } {
        background = "\"#000000\"";
        fuzzy_edges = "3";
        geometry = "1x1+10+742";
        grow_gravity = "SW";
        icon_gravity = "SW";
        icon_size = "16";
        skip_taskbar = true;
        sticky = true;
        transparent = true;
        window_layer = "bottom";
        window_strut = "bottom";
        window_type = "desktop";
      };

    keyboard = {
      layout = "dvorak,ru";
      options = ["ctrl:swapcaps" "grp:shifts_toggle"];
      variant = ",ruu";
    };

    language = listToAttrs (map (n: nameValuePair n "en_US.UTF-8") [
      "address"
      "base"
      "collate"
      "ctype"
      "measurement"
      "messages"
      "monetary"
      "name"
      "numeric"
      "paper"
      "telephone"
      "time"
    ]);

    sessionVariables = {
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
      ADB_VENDOR_KEY = "${config.xdg.cacheHome}/android";
      ANDROID_SDK_HOME = "${config.xdg.cacheHome}/android";
      BOOT_HOME = "${config.xdg.cacheHome}/boot";
      BROWSER = "browser";
      CARGO_HOME = "${config.xdg.cacheHome}/cargo";
      CUDA_CACHE_PATH = "${config.xdg.cacheHome}/nv";
      DOTNET_CLI_TELEMETRY_OPTOUT = "1";
      EDITOR = "emacs";
      GEM_HOME = "${config.xdg.cacheHome}/gem";
      GEM_SPEC_CACHE = "${config.xdg.cacheHome}/gem";
      GTK_IM_MODULE = "ibus";
      LESSHISFILE = "/dev/null";
      MAILDIR = config.accounts.email.maildirBasePath;
      MPD_HOST = "localhost";
      MPD_PORT = "6600";
      NPM_CONFIG_USERCONFIG = "${config.xdg.configHome}/npm/npmrc";
      QT_IM_MODULE = "ibus";
      RUSTUP_HOME = "${config.xdg.cacheHome}/rustup";
      SSB_HOME = "${config.xdg.cacheHome}/zoom";
      SUDO_ASKPASS = "${pkgs.sudo_askpass}/bin/sudo_askpass";
      TERMINAL = "uxterm";
      VISUAL = config.home.sessionVariables.EDITOR;
      WGETRC = "${config.xdg.configHome}/wgetrc";
      XAUTHORITY = "\${XDG_RUNTIME_DIR}/Xauthority";
      XMODIFIERS = "ibus";
      YTDL_DIR = "${config.xdg.userDirs.videos}/youtube";
    };

    stateVersion = "18.09";
  };
}
