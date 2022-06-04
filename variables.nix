# -*- eval: (rainbow-mode); -*-
let user = "val";
in {
  inherit user;
  dir = {
    config = "/home/${user}/.config";
    data = "/home/${user}/.local/share";
    cache = "/home/${user}/.cache";
    music = "/home/${user}/Music";
    videos = "/home/${user}/Videos";
    pictures = "/home/${user}/Pictures";
    mail = "/home/${user}/.mail";
  };
  colors = let
    themes = {
      summerfruitLight = {
        base00 = "#FFFFFF";
        base01 = "#E0E0E0";
        base02 = "#D0D0D0";
        base03 = "#B0B0B0";
        base04 = "#000000";
        base05 = "#101010";
        base06 = "#151515";
        base07 = "#202020";
        base08 = "#FF0086";
        base09 = "#FD8900";
        base0A = "#ABA800";
        base0B = "#00C918";
        base0C = "#1FAAAA";
        base0D = "#3777E6";
        base0E = "#AD00A1";
        base0F = "#CC6633";
      };
      gruvboxLightMedium = {
        base00 = "#fbf1c7";
        base01 = "#ebdbb2";
        base02 = "#d5c4a1";
        base03 = "#bdae93";
        base04 = "#665c54";
        base05 = "#504945";
        base06 = "#3c3836";
        base07 = "#282828";
        base08 = "#9d0006";
        base09 = "#af3a03";
        base0A = "#b57614";
        base0B = "#79740e";
        base0C = "#427b58";
        base0D = "#076678";
        base0E = "#8f3f71";
        base0F = "#d65d0e";
      };
    }; in themes.gruvboxLightMedium;
}
