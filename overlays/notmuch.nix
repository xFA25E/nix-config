self: super: {
  notmuch = (super.notmuch.override {
    withEmacs = false;
  }).overrideAttrs (oldAttrs: {
    version = "0.34.2";
    src = self.fetchgit {
      url = "https://git.notmuchmail.org/git/notmuch";
      rev = "a254a15861d3510adbe2897fed100a3c77642165";
      sha256 = "1sn6qb2d7rr7jnlr3vyfcvlzzi7b1l1p0mi2s7nghv8x59b5dqp4";
    };
  });
}
