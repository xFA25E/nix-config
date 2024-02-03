{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./config.nix];

  xdg = {
    enable = true;

    configFile."flameshot/flameshot.ini".text = lib.generators.toINI {} {
      General.savePath = "${config.xdg.userDirs.pictures}/screenshots";
    };

    dataFile = {
      "applications/browser.desktop".text = lib.generators.toINI {} {
        "Desktop Entry" = {
          Categories = "Network;WebBrowser;";
          Comment = "";
          Exec = "${pkgs.browser}/bin/browser %U";
          GenericName = "Web Browser";
          MimeType = "text/html;text/xml;application/xhtml+xml;x-scheme-handler/http;x-scheme-handler/https;x-scheme-handler/ftp";
          Name = "Browser";
          Terminal = false;
          Type = "Application";
        };
      };

      "stardict/dic".source = pkgs.stardicts;
    };

    mimeApps = {
      enable = true;
      associations = {
        added = {
          "application/pdf" = ["emacs.desktop"];
          "application/epub" = ["emacs.desktop"];
        };
        removed = {};
      };
      defaultApplications = {
        "application/pdf" = ["emacs.desktop"];
        "application/epub" = ["emacs.desktop"];
        "text/html" = ["browser.desktop"];
        "x-scheme-handler/https" = ["browser.desktop"];
        "x-scheme-handler/http" = ["browser.desktop"];
      };
    };

    userDirs = {
      enable = true;
      createDirectories = true;
      desktop = "${config.home.homeDirectory}/Desktop";
      documents = "${config.home.homeDirectory}/Documents";
      download = "${config.home.homeDirectory}/Downloads";
      music = "${config.home.homeDirectory}/Music";
      pictures = "${config.home.homeDirectory}/Pictures";
      videos = "${config.home.homeDirectory}/Videos";
    };
  };
}
