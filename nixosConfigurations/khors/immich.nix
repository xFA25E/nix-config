{config, ...}: {
  services.immich.enable = true;
  systemd.tmpfiles.rules = ["d /var/lib/immich-external-libraries 0755 immich immich -"];
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;
    virtualHosts."immich.litkov.one" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.immich.port}/";
        proxyWebsockets = true;
      };
      extraConfig = ''
        client_max_body_size 50000M;
        proxy_request_buffering off;
        client_body_buffer_size 1024k;

        proxy_read_timeout 600s;
        proxy_send_timeout 600s;
        send_timeout       600s;

        client_body_timeout 1h;
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
      '';
    };
  };
}
