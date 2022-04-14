self: super: {
  notmuch = (super.notmuch.override {
    withEmacs = false;
  }).overrideAttrs (oldAttrs: {
    version = "0.34.2";
    src = self.fetchgit {
      url = "https://git.notmuchmail.org/git/notmuch";
      rev = "79dc8a54f16037a5d2c0b4b1b036941bad4f62d8";
      sha256 = "14jf8plnciwzvchxpz0dliskljb03m8jmrj44bwcvr9pqsf6zgq8";
    };
  });
}
