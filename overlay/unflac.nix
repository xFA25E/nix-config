{
  buildGo118Module,
  src,
}:
buildGo118Module {
  name = "unflac";
  inherit src;
  vendorSha256 = "sha256-DHgE6QWzcgzvSF2hzLvnfzQeWLQPJxy1d2vjfxWc+Vo=";
  proxyVendor = true;
}
