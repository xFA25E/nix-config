{
  config,
  pkgs,
  ...
}: let
  head = "${pkgs.coreutils}/bin/head";
  pass = "${config.programs.password-store.package}/bin/pass";

  makeChannel = far: near: {
    extraConfig = {
      Create = "Near";
      Sync = "Full";
      Expunge = "Both";
      SyncState = "*";
    };
    farPattern = far;
    nearPattern = near;
  };
in {
  accounts.email = {
    accounts = {
      "litkov" = {
        address = "valeriy@litkov.one";
        aliases = ["valery@litkov.one"];

        imap = {
          host = "mail.litkov.one";
          tls.enable = true;
        };

        mbsync = {
          enable = true;
          groups."litkov".channels = {
            "archive" = makeChannel "Archive" "archive";
            "drafts" = makeChannel "Drafts" "drafts";
            "flagged" = makeChannel "Flagged" "flagged";
            "inbox" = makeChannel "INBOX" "inbox";
            "sent" = makeChannel "Sent" "sent";
            "spam" = makeChannel "Junk" "spam";
            "trash" = makeChannel "Trash" "trash";
          };
        };

        msmtp = {
          enable = true;
          extraConfig.logfile = "${config.xdg.cacheHome}/msmtp-litkov.log";
        };

        notmuch.enable = true;
        passwordCommand = "${pass} show khors/val";

        primary = true;
        realName = "Valeriy Litkovskyy";

        smtp = {
          host = "mail.litkov.one";
          tls.enable = true;
        };

        userName = "valeriy@litkov.one";
      };

      "polimi" = {
        address = "valeriy.litkovskyy@mail.polimi.it";
        aliases = ["10622800@polimi.it"];

        imap = {
          host = "outlook.office365.com";
          tls.enable = true;
        };

        mbsync = {
          enable = false;
          groups."polimi" = {
            channels = {
              "inbox" = makeChannel "INBOX" "inbox";
              "sent" = makeChannel "Sent Items" "sent";
            };
          };
        };

        msmtp = {
          enable = false;
          extraConfig.logfile = "${config.xdg.cacheHome}/msmtp-polimi.log";
        };

        notmuch.enable = true;
        passwordCommand = "${pass} show mail/polimi | ${head} -n1";

        realName = "Valeriy Litkovskyy";

        smtp = {
          host = "smtp.office365.com";
          tls = {
            enable = true;
            useStartTls = true;
          };
        };

        userName = "10622800@polimi.it";
      };

      "nonsolocodice" = {
        address = "vl@nonsolocodice.it";
        aliases = [];

        imap = {
          host = "pop.tophost.it";
          tls.enable = true;
        };

        mbsync = {
          enable = true;
          groups."nonsolocodice".channels = {
            "inbox" = makeChannel "INBOX" "inbox";
            "sent" = makeChannel "Sent" "sent";
          };
        };

        msmtp = {
          enable = true;
          extraConfig.logfile = "${config.xdg.cacheHome}/msmtp-nonsolocodice.log";
        };

        notmuch.enable = true;
        passwordCommand = "${pass} show nonsolocodice/mail";

        realName = "Valeriy Litkovskyy";

        smtp = {
          host = "mail.tophost.it";
          tls.enable = true;
        };

        userName = "nonsolocodice.it38613";
      };
    };
    maildirBasePath = "${config.xdg.dataHome}/mail";
  };
}
