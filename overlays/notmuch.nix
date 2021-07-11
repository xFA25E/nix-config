self: super: {
  notmuch = (super.notmuch.override {
    withEmacs = false;
  }).overrideAttrs (oldAttrs: {
    version = "0.32.2";
    src = self.fetchgit {
      url = "https://git.notmuchmail.org/git/notmuch";
      sha256 = "1a1l2w4bas7vi9h9if6c3ah0xh3ky39pkrk0lmc666assp1shamd";
      rev = "04f378e673852ade100c54318124ff8c22f857b6";
    };
  });
}
