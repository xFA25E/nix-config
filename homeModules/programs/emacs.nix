{
  pkgs,
  inputs,
  ...
}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./../xdg/emacs/init.el;
      package = pkgs.emacs;
      extraEmacsPackages = epkgs:
        map (flake: inputs."epkg-${flake}".packages.${pkgs.system}.default) [
          "amded"
          "cyrillic-dvorak-im"
          "dired-atool-transient"
          "dired-tags"
          "pueue"
          "rx-widget"
          "sdcwoc"
          "tempo-extra"
        ];
    };
  };
}
