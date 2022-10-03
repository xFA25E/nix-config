{
  fetchurl,
  imagemagick,
  runCommand,
}: let
  wallpapers = map fetchurl (import ./urls.nix);
in
  runCommand "wallpapers" {
    srcs = builtins.tail wallpapers;
    pepe = builtins.head wallpapers;
    nativeBuildInputs = [imagemagick];
  } ''
    mkdir -p $out
    for src in $srcs; do ln -s $src $out/$(stripHash $src); done
    convert $pepe -resize 1366x -crop 1366x768+0+200 $out/pepefishing.jpg
  ''
