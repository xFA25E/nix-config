{pkgs, ...}: {
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    virtualHosts."sincere.litkov.one" = {
      forceSSL = true;
      enableACME = true;
      root = pkgs.sincere-blog;
    };
  };
}
