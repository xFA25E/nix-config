self: super: {
  rimer = super.rustPlatform.buildRustPackage rec {
    pname = "rimer";
    version = "";
    src = super.fetchFromGitHub {
      owner = "xFA25E";
      repo = pname;
      rev = "3d87266899b4f679573b19d707fb2aa9dedadcb1";
      sha256 = "1l26sw3ysfbzbd7wav2qixwfla3qg77sgmqjd3dv0n1yiv1fdfb1";
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
