self: super: {
  sctd = super.rustPlatform.buildRustPackage rec {
    pname = "sctd";
    version = "0.2.0";
    src = super.fetchFromGitHub {
      owner = "amir";
      repo = pname;
      rev = version;
      sha256 = "17qzi7i12yxb7cxwgax6d93mg4lvzmm4v3b3yra63w7287gn5xjh";
    };
    nativeBuildInputs = [ self.pkgconfig ];
    buildInputs = [ self.xorg.libX11 self.xorg.libXrandr ];
    cargoSha256 = "0wchlkf43w8rig4z31z09vk7f4fimia6a1aajxmf2csz0g2c6hi1";
    meta = with super.lib; {
      description = "set color temperature daemon";
      homepage = "https://github.com/amir/sctd";
      license = licenses.cc0;
      maintainers = [ "Amir Saeid <amir@glgdgt.com>" ];
    };
  };
}
