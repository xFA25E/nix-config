self: super: {
  dmenu = super.dmenu.overrideAttrs (attrs: {
    patches = attrs.patches ++ [ ./dmenu.patch ];
  });
}
