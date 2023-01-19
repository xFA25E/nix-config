{
  clojure = {
    description = "Simple clojure environment";
    path = ./clojure;
    welcomeText = ''
      Welcome to clojure development environment!

      !!! REMEMBER TO RUN LEIN NEW !!!
    '';
  };
  csharp = {
    description = "Simple C# environment";
    path = ./csharp;
    welcomeText = ''
      Welcome to C# development environment!

      !!! REMEMBER !!!

      $ dotnet new <template> -n <name> -o src

      $ dotnet new gitignore

      $ dotnet new sln

      $ dotnet sln add src

      $ dotnet format --include-generated *.sln

      $ dos2unix *.sln src/*.csproj .gitignore
    '';
  };
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
