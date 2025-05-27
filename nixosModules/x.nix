{username, ...}: {
  programs.dconf.enable = true;

  qt = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };

  services = {
    displayManager.defaultSession = "none";
    libinput.enable = true;

    xserver = {
      enable = true;
      xkb = {
        layout = "us,ru";
        options = "ctrl:swapcaps,grp:shifts_toggle";
        variant = "dvorak,ruu";
      };
      displayManager.startx.enable = true;
    };
  };

  users.users.${username}.extraGroups = ["video" "audio"];
}
