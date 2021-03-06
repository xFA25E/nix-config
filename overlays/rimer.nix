self: super: {
  rimer = super.rustPlatform.buildRustPackage rec {
    pname = "rimer";
    version = "";
    src = super.fetchFromGitHub {
      owner = "xFA25E";
      repo = pname;
      rev = "ad1dbbef0a116cc115997d54041bec13f69fe43c";
      sha256 = "061nfws326h4dml7rlr2i1qc9a5xmf8lpkbl81z42v5abzc6f05x";
    };
    doCheck = false;
    cargoSha256 = "1r03334c8y5kj102cz2f9x57h1v3z3dw7nxhjm7gpin16lwvd5ca";
    meta = with super.lib; {
      description = "Simple timer that executes commands on update";
      homepage = "https://github.com/xFA25E/rimer";
      license = licenses.unlicense;
      maintainers = [ "Valeriy Litkovskyy" ];
    };
  };
}
