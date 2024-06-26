{config, ...}: let
  inherit (config.colorScheme) palette;
in {
  xresources.properties = {
    "Nsxiv.window.background" = "#${palette.base00}";
    "Nsxiv.bar.font" = "monospace-9";
    "Nsxiv.window.foreground" = "#${palette.base05}";
    "dmenu.font" = "monospace:size=10";
    "dmenu.normbgcolor" = "#${palette.base00}";
    "dmenu.normfgcolor" = "#${palette.base05}";
    "dmenu.selbgcolor" = "#${palette.base01}";
    "dmenu.selfgcolor" = "#${palette.base05}";
    "xterm*background" = "#${palette.base00}";
    "xterm*color0" = "#${palette.base00}";
    "xterm*color1" = "#${palette.base08}";
    "xterm*color10" = "#${palette.base0B}";
    "xterm*color11" = "#${palette.base0A}";
    "xterm*color12" = "#${palette.base0D}";
    "xterm*color13" = "#${palette.base0E}";
    "xterm*color14" = "#${palette.base0C}";
    "xterm*color15" = "#${palette.base07}";
    "xterm*color16" = "#${palette.base09}";
    "xterm*color17" = "#${palette.base0F}";
    "xterm*color18" = "#${palette.base01}";
    "xterm*color19" = "#${palette.base02}";
    "xterm*color2" = "#${palette.base0B}";
    "xterm*color20" = "#${palette.base04}";
    "xterm*color21" = "#${palette.base06}";
    "xterm*color3" = "#${palette.base0A}";
    "xterm*color4" = "#${palette.base0D}";
    "xterm*color5" = "#${palette.base0E}";
    "xterm*color6" = "#${palette.base0C}";
    "xterm*color7" = "#${palette.base05}";
    "xterm*color8" = "#${palette.base03}";
    "xterm*color9" = "#${palette.base08}";
    "xterm*cursorBlink" = "false";
    "xterm*cursorColor" = "#${palette.base08}";
    "xterm*eightBitInput" = "false";
    "xterm*font" = "xft:monospace:size=10";
    "xterm*foreground" = "#${palette.base05}";
    "xterm*internalBorder" = 0;
    "xterm*saveLines" = 10240;
    "xterm*scrollTtyOutput" = "false";
    "xterm*termName" = "xterm-color";
  };
}
