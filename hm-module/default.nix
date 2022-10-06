{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) attrNames length;
  inherit (lib) types;
  inherit (lib.attrsets) mapAttrsToList;
  inherit (lib.lists) flatten optional;
  inherit (lib.strings) makeBinPath;
  inherit (pkgs) babashka mozlz4 writeShellScript;

  isAttrsetEmpty = a: (0 != length (attrNames a));
  addToSearchEngines = writeShellScript "add-to-search-engines" ''
    set -eu
    export PATH="${makeBinPath [babashka mozlz4]}:$PATH"
    (rm "$1" && mozlz4 -x - | bb -f "${./addToSearchEngines.clj}" "$2" | mozlz4 -z - >"$1") <"$1"
  '';
in {
  options.programs.firefox.profiles = lib.mkOption {
    type = types.attrsOf (types.submodule {
      options.searchEngines = lib.mkOption {
        type = types.attrsOf (types.submodule {
          options = {
            keyword = lib.mkOption {
              type = types.str;
            };
            url = lib.mkOption {
              type = types.str;
              description = "Use {searchTerms}";
            };
          };
        });
        default = {};
      };
    });
  };

  config = lib.mkIf config.programs.firefox.enable {
    home.file = lib.mkMerge (flatten (
      mapAttrsToList
      (name: value:
        optional (isAttrsetEmpty value.searchEngines)
        {
          ".mozilla/firefox/${name}/hmSearchEngines.json" = {
            text = builtins.toJSON value.searchEngines;
            onChange = ''
              ${addToSearchEngines} \
                "$HOME/.mozilla/firefox/${name}/search.json.mozlz4" \
                "$HOME/.mozilla/firefox/${name}/hmSearchEngines.json"
            '';
          };
        })
      config.programs.firefox.profiles
    ));
  };
}
