{config, ...}: {
  networking.firewall.allowedTCPPorts = with config.services.syncthing.relay; [port statusPort];
  services.syncthing.relay = {
    enable = true;
    providedBy = "Valeriy Litkovskyy";
    globalRateBps = 100 * 1000 * 1000;
    perSessionRateBps = 10 * 1000 * 1000;
  };

  systemd.services.syncthing-relay.serviceConfig = {
    ProtectSystem = "strict";
    ProtectHome = true;
    PrivateTmp = true;
    PrivateDevices = true;
    NoNewPrivileges = true;
    LockPersonality = true;
    RestrictSUIDSGID = true;
    RestrictRealtime = true;
    RestrictAddressFamilies = ["AF_INET" "AF_INET6" "AF_UNIX"];
    CapabilityBoundingSet = "";
    AmbientCapabilities = "";
  };
}
