final: prev: efinal: eprev:
efinal.elpaBuild {
  pname = "project";
  ename = "project";
  version = "0.9.8";
  src = final.fetchurl {
    url = "http://elpa.gnu.org/packages/project-0.9.8.tar";
    hash = "sha256-3d805P9weC2QyjtdTNEKoGZh8QT7oQTl82vIu+lKOEQ=";
  };
  packageRequires = [efinal.emacs efinal.xref];
}
