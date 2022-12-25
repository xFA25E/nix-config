final: prev:
prev.mpvScripts
// (
  let
    inherit (builtins) attrNames filter listToAttrs readDir;
    inherit (final.lib.attrsets) nameValuePair;

    callFile = file: import "${./.}/${file}" final prev;
    removeNix = final.lib.strings.removeSuffix ".nix";

    scriptFiles = filter (f: f != "default.nix") (attrNames (readDir ./.));
  in
    listToAttrs (map (f: nameValuePair (removeNix f) (callFile f)) scriptFiles)
)
