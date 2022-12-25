pkgs: let
  emacsPackages = pkgs.emacsPackagesFor pkgs.emacsNativeComp;

  inherit (builtins) attrNames elem filter readDir;
  inherit (pkgs.lib.attrsets) filterAttrs mapAttrs recurseIntoAttrs;
  inherit (pkgs.lib.strings) removeSuffix;

  dirFiles = dir: filter (f: f != "default.nix") (attrNames (readDir dir));
  dirNames = dir: map (removeSuffix ".nix") (dirFiles dir);
  filterSet = set: names: filterAttrs (name: _: elem name names) set;

  packageNames = attrNames (import ./overlay null null null);
  emacsPackageNames = dirNames ./overlay/epkgs;
  mpvScriptNames = dirNames ./overlay/mpvScripts;
in
  filterSet pkgs packageNames
  // mapAttrs (name: recurseIntoAttrs) {
    emacsPackages = filterSet emacsPackages emacsPackageNames;
    mpvScripts = filterSet pkgs.mpvScripts mpvScriptNames;
  }
