{
  config,
  pkgs,
  ...
}: let
  head = "${pkgs.coreutils}/bin/head";
  pass = "${config.programs.password-store.package}/bin/pass";

  makeChannel = far: near: {
    extraConfig = {
      Create = "Slave";
      Sync = "All";
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

        msmtp = {
          enable = true;
          extraConfig.logfile = "${config.xdg.cacheHome}/msmtp-litkov.log";
        };

        passwordCommand = "${pass} show litkov.one/val";
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
          enable = true;
          groups."polimi" = {
            channels = {
              "inbox" = makeChannel "INBOX" "inbox";
              "sent" = makeChannel "Sent Items" "sent";
            };
          };
        };

        msmtp = {
          enable = true;
          extraConfig.logfile = "${config.xdg.cacheHome}/msmtp-polimi.log";
        };

        notmuch.enable = true;
        passwordCommand = "${pass} show mail/polimi | ${head} -n1";

        primary = true;
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
    };
    maildirBasePath = "${config.xdg.dataHome}/mail";
  };
}
