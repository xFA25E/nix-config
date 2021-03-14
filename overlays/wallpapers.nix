self: super: let
  ids = [
    "0pgepm" "4dy2jm" "nelq58" "nk5r5d" "43jgvv" "nkrqm1" "nelelw" "0qy3dn"
    "n6616x" "47qm53" "nrzp9w" "nmer59" "48mpzj" "n6z6jq" "d5wwy3" "nkkv8d"
    "0j75l5"
  ];
  makeUrl = id: "https://w.wallhaven.cc/full/${builtins.substring 0 2 id}/wallhaven-${id}.jpg";
  urls = map makeUrl ids;
in {
  # convert orig -resize 1366x -crop 1366x768+0+200 result
  wallpapers = super.runCommand "wallpapers" {
    srcs = urls ++ [ "https://i.kym-cdn.com/photos/images/original/001/877/857/ff1.jpg" ];
  } ''
    ${self.wget}/bin/wget --no-check-certificate --directory-prefix=$out $srcs
    ${self.imagemagick}/bin/convert $out/ff1.jpg -resize 1366x -crop 1366x768+0+200 $out/fishing.jpg
    rm $out/ff1.jpg
  '';
}
