final: prev:
prev.dmenu.override {
  patches = [
    (final.fetchpatch {
      url = "https://tools.suckless.org/dmenu/patches/case-insensitive/dmenu-caseinsensitive-5.0.diff";
      sha256 = "sha256-XqFEBRu+aHaAXrNn+WXnkIuC/vAHDIb/im2UhjaPYC0=";
    })

    (
      final.runCommand "dmenu-xresources-alt-5.0.diff" {
        src = final.fetchpatch {
          url = "https://tools.suckless.org/dmenu/patches/xresources-alt/dmenu-xresources-alt-5.0.diff";
          sha256 = "sha256-znx7FoXnddHWsZaB2liTNhM/szktnlgecDwVhax6waA=";
        };
      } ''
        sed 83,91d $src > $out
      ''
    )
  ];
}
