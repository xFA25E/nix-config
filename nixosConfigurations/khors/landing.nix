{pkgs, ...}: let
  landingPage = pkgs.linkFarm "make-landing" [
    {
      name = "index.html";
      path = ./index.html;
    }
  ];
in {
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    virtualHosts."litkov.one" = {
      forceSSL = true;
      enableACME = true;
      root = landingPage;
    };
  };
}
