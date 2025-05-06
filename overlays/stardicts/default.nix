{
  fetchurl,
  lib,
  runCommand,
  util-linux,
}:
runCommand "stardicts" {
  nativeBuildInputs = [util-linux];
  # srcs = map fetchurl (builtins.fromJSON (builtins.readFile ./urls.json));
  srcs = [];
} ''
  mkdir $out
  for src in $srcs; do
    tar -xj -C $out -f $src
  done
  # rename stardict- "" $out/*
''
