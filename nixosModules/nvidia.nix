{pkgs, ...}: {
  hardware = {
    graphics = {
      enable = true;
      # extraPackages = with pkgs; [nvidia-vaapi-driver];
    };
    nvidia = {
      modesetting.enable = true;
      open = true;
      nvidiaSettings = true;
    };
  };

  services.xserver.videoDrivers = ["nvidia"];
}
