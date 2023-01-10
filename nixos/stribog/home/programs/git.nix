{pkgs, ...}: {
  programs.git = {
    enable = true;
    attributes = [
      "*.bib      diff=bibtex"
      "*.c        diff=cpp"
      "*.h        diff=cpp"
      "*.c++      diff=cpp"
      "*.h++      diff=cpp"
      "*.cpp      diff=cpp"
      "*.hpp      diff=cpp"
      "*.cc       diff=cpp"
      "*.hh       diff=cpp"
      "*.cs       diff=csharp"
      "*.css      diff=css"
      "*.ex       diff=elixir"
      "*.exs      diff=elixir"
      "*.f        diff=fortran"
      "*.fountain diff=fountain"
      "*.go       diff=golang"
      "*.html     diff=html"
      "*.xhtml    diff=html"
      "*.java     diff=java"
      "*.kt       diff=kotlin"
      "*.kts      diff=kotlin"
      "*.ktm      diff=kotlin"
      "*.md       diff=markdown"
      "*.m        diff=matlab"
      "*.pl       diff=perl"
      "*.php      diff=php"
      "*.py       diff=python"
      "*.rb       diff=ruby"
      "*.rs       diff=rust"
      "*.scm      diff=scheme"
      "*.tex      diff=tex"

      "*.el    diff=elisp"
      "*.lisp  diff=lisp"
      "*.org   diff=org"
      "*.r     diff=rstats"
      "*.texi* diff=texinfo"

      "*.png  diff=exif"
      "*.jpg  diff=exif"
      "*.jpeg diff=exif"
      "*.gif  diff=exif"
      "*.pdf  diff=pdf"
    ];
    extraConfig = {
      credential.helper = "cache --timeout=86400";
      diff = {
        elisp.xfuncname = "^((;;;+|\\((cl-)?def\\S+)\\s+.*)";
        lisp.xfuncname = "^\\((def\\S+\\s+.*)";
        org.xfuncname = "^(\\*+\\s+.*)";
        rstats.xfuncname = "^([a-zA-z.]+ <- function.*)$";
        texinfo.xfuncname = "^@node[ \t][ \t]*\\([^,][^,]*\\)";

        exif = {
          binary = true;
          textconv = "${pkgs.exiftool}/bin/exiftool";
        };
        pdf.textconv = toString (pkgs.writeShellScript "pdftotext-stdout" ''
          ${pkgs.poppler_utils}/bin/pdftotext "$@" -
        '');
      };
    };
    ignores = [
      "*-autoloads.el"
      "*.dx32fsl"
      "*.dx64fsl"
      "*.elc"
      "*.fasl"
      "*.lx32fsl"
      "*.lx64fsl"
      "*.o"
      "*.so"
      "*.x86f"
      "*~"
      ".#*"
      ".common-lisp"
      ".direnv/"
      ".eldev/"
      ".envrc"
      ".quicklisp"
      "Eldev-local"
      "result"
    ];
    signing = {
      key = "Litkovskyy Valeriy <vlr.ltkvsk@protonmail.com>";
      signByDefault = true;
    };
    userEmail = "vlr.ltkvsk@protonmail.com";
    userName = "Valeriy Litkovskyy";
  };
}
