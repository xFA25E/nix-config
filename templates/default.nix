{
  emacs = {
    description = "Simple emacs-lisp environment";
    path = ./emacs;
    welcomeText = ''
      Welcome to emacs-lisp development environment!

      !!! REMEMBER TO CUSTOMIZE PACKAGE-NAME THROUGHOUT THE PROJECT !!!
    '';
  };
  lisp = {
    description = "Simple common-lisp environment";
    path = ./lisp;
    welcomeText = ''
      Welcome to common-lisp development environment!
    '';
  };
}
