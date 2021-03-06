self: super: {
  dmenu = super.dmenu.override { patches = [ ./dmenu.patch ]; };
}
