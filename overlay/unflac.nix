{
  buildGoModule,
  src,
}:
buildGoModule {
  name = "unflac";
  inherit src;
  vendorSha256 = "sha256-R5Sa7pYRg79tkZ0jsupjvJVZ6D5jqN1syPz/YR5wF8g=";
  proxyVendor = true;
}
