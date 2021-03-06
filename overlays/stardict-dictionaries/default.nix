self: super: {
  stardictDictionaries = super.runCommand "stardict-dictionaries" {
    srcs = let
      removeSuffix = super.lib.strings.removeSuffix;
      getName = dic: removeSuffix ".tar.bz2" (baseNameOf dic.url);
      makeAttrs = dic: dic // { name = getName dic; };
      fetch = dic: super.fetchzip (makeAttrs dic);
    in map fetch (import ./urls.nix);
  } ''
    mkdir -p "$out/share/stardict/dic"
    for src in $srcs; do
        ln -s "$src" "$out/share/stardict/dic/$(stripHash $src)"
    done
  '';
}
