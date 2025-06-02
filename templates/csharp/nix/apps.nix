pkgs: {
  updateNugetDeps = {
    type = "app";
    program = toString (pkgs.writeShellScript "updateNugetDeps" ''
      ${pkgs.package.fetch-deps} "''${PRJ_ROOT:?}/nix/package/deps.json"
    '');
  };
}
