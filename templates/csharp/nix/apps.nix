pkgs: {
  updateNugetDeps = {
    type = "app";
    program = toString (pkgs.writeShellScript "updateNugetDeps" ''
      ${pkgs.package.fetch-deps} "''${PROJECT_ROOT:?}/nix/package/deps.json"
    '');
  };
}
