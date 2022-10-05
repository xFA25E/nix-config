{
  fetchFromGitHub,
  rustPlatform,
  src,
}:
rustPlatform.buildRustPackage {
  inherit src;
  pname = "mozlz4";
  version = "0.1.0";
  cargoSha256 = "sha256-8FXjJ6hLJk9H5VEmjD1vM81lZkB8ZYFiypjkgTzZhNU=";
}
