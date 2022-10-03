{
  fetchzip,
  lib,
  runCommand,
}: let
  makeName = url:
    lib.trivial.pipe url [
      baseNameOf
      (lib.strings.removePrefix "stardict-")
      (lib.strings.removeSuffix ".tar.bz2")
    ];

  addName = dict: dict // {name = makeName dict.url;};
  dicts = map addName (import ./urls.nix);
in
  runCommand "stardicts" {srcs = map fetchzip dicts;} ''
    mkdir -p "$out/share/stardict/dic"
    for src in $srcs; do
      ln -s "$src" "$out/share/stardict/dic/$(stripHash $src)"
    done
  ''
