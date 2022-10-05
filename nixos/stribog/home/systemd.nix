{pkgs, ...}: {
  systemd.user.services = {
    pueue = {
      Unit = {
        Description = "Pueue Daemon - CLI process scheduler and manager";
        After = ["graphical-session-pre.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        Restart = "no";
        ExecStart = "${pkgs.pueue}/bin/pueued";
        ExecReload = "${pkgs.pueue}/bin/pueued";
        Environment = "ASYNC_STD_THREAD_COUNT=4";
      };
      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };

    transmission = {
      Unit = {
        Description = "Transmission BitTorrent Daemon";
        After = ["network.target" "graphical-session-pre.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        Type = "notify";
        ExecStart = "${pkgs.transmission}/bin/transmission-daemon -f --log-error";
        ExecReload = "${pkgs.utillinux}/bin/kill -s HUP $MAINPID";
        NoNewPrivileges = true;
      };
      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };

    udiskie.Service.Environment = "PATH=${pkgs.dbus}/bin:${pkgs.systemd}/bin";
  };
}
