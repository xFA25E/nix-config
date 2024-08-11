final: prev: emacs:
(prev.emacsPackagesFor emacs).overrideScope (
  efinal: eprev: let
    inherit (builtins) attrNames filter listToAttrs readDir;
    inherit (final.lib.attrsets) nameValuePair;

    callFile = file: import "${./.}/${file}" final prev efinal eprev;
    removeNix = final.lib.strings.removeSuffix ".nix";

    epkgFiles = filter (f: f != "default.nix") (attrNames (readDir ./.));
  in
    listToAttrs (map (f: nameValuePair (removeNix f) (callFile f)) epkgFiles)
)
