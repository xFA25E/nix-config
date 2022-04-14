self: super: {
  tdlib = super.tdlib.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "tdlib";
      repo = "td";
      rev = "v1.8.0";
      sha256 = "19psqpyh9a2kzfdhgqkirpif4x8pzy89phvi59dq155y30a3661q";
    };
  });
}
