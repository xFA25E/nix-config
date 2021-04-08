self: super: {
  unflac = super.buildGoModule {
    name = "unflac";

    src = super.fetchgit {
      url = "https://git.sr.ht/~ft/unflac";
      rev = "d677c2e4135927b22300bd49a7fb0f1f140e1660";
      sha256 = "1agwzfaw5xl5ncq6f8dazsfcznpg12lh6acp818g5s2xs4gj3dis";
    };

    vendorSha256 = "0mjhbswrw2zjm86klz0ljrcnsgvcqw577kf8fzr5ylrxm28gy6bj";

    subPackages = [ "." ];

    runVend = true;

  };
}
