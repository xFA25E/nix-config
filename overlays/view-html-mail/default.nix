{
  python3Packages,
  xdg-utils,
}:
python3Packages.buildPythonApplication {
  pname = "view-html-mail";
  version = "0.0.1";
  pyproject = false;
  propagatedBuildInputs = [xdg-utils];
  dontUnpack = true;
  installPhase = ''
    install -Dm755 "${./view-html-mail.py}" "$out/bin/view-html-mail"
  '';
}
