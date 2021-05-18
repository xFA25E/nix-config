self: super: {
  cpulimit = super.cpulimit.overrideAttrs (oldAttrs: {
    src =  super.fetchFromGitHub {
      owner = "opsengine";
      repo = "cpulimit";
      rev = "f4d2682804931e7aea02a869137344bb5452a3cd";
      sha256 = "1dz045yhcsw1rdamzpz4bk8mw888in7fyqk1q1b3m1yk4pd1ahkh";
    };
    buildPhase = ''
    cd src && make
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp cpulimit $out/bin/
    '';
  });
}
