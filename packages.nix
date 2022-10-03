pkgs: let
  epkgs = pkgs.emacsPackagesFor pkgs.emacsNativeComp;

  inherit (builtins) attrNames elem filter readDir;
  inherit (pkgs.lib.attrsets) filterAttrs mapAttrs' nameValuePair;
  inherit (pkgs.lib.strings) removeSuffix;

  dirFiles = dir: filter (f: f != "default.nix") (attrNames (readDir dir));

  oPkgNames = attrNames (import ./overlay null null null);
  oEpkgNames = map (removeSuffix ".nix") (dirFiles ./overlay/epkgs);

  oPkgs = filterAttrs (name: _: elem name oPkgNames) pkgs;
  ePkgs = filterAttrs (name: _: elem name oEpkgNames) epkgs;
in
  oPkgs // mapAttrs' (name: nameValuePair "epkg-${name}") ePkgs
