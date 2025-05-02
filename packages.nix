pkgs: let
  inherit (builtins) attrNames elem filter readDir;
  inherit (pkgs.lib.attrsets) filterAttrs mapAttrs recurseIntoAttrs;
  inherit (pkgs.lib.strings) removeSuffix;

  dirFiles = dir: filter (f: f != "default.nix") (attrNames (readDir dir));
  dirNames = dir: map (removeSuffix ".nix") (dirFiles dir);
  filterSet = set: names: filterAttrs (name: _: elem name names) set;

  packageNames = attrNames ((import ./overlays null).default null null);
  emacsPackageNames = dirNames ./overlays/emacsPackages;
  mpvScriptNames = dirNames ./overlays/mpvScripts;
in
  filterSet pkgs packageNames
  // mapAttrs (name: recurseIntoAttrs) {
    emacsPackages = filterSet (pkgs.emacsPackagesFor pkgs.emacs) emacsPackageNames;
  }
