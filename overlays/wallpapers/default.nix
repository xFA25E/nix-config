{
  fetchurl,
  imagemagick,
  runCommand,
}: let
  inherit (builtins) fromJSON readFile;
  wallpapers = map fetchurl (fromJSON (readFile ./urls.json));
in
  runCommand "wallpapers" {
    srcs = builtins.tail wallpapers;
    pepe = builtins.head wallpapers;
    nativeBuildInputs = [imagemagick];
  } ''
    mkdir -p $out
    for src in $srcs; do ln -s $src $out/$(stripHash $src); done
    magick convert $pepe -resize 1366x -crop 1366x768+0+200 $out/pepefishing.jpg
  ''
