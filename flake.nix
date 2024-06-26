{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... } @ inputs:
    let
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = f: builtins.listToAttrs (map (name: { inherit name; value = f name; }) systems);
      getErlangLibs = erlangPkg: 
        let
            erlangPath = "${erlangPkg}/lib/erlang/lib/";
            dirs = builtins.attrNames (builtins.readDir erlangPath);
            interfaceVersion = builtins.head (builtins.filter (s: builtins.substring 0 13 s == "erl_interface") dirs);
            interfacePath = erlangPath + interfaceVersion;
        in
        {
            path = erlangPath;
            dirs = dirs;
            interface = { version = interfaceVersion; path = interfacePath; };
        };

      mkEnvVars = pkgs: erlangLatest: erlangLibs: {
        LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
        LANG = "en_US.UTF-8";
        # https://www.erlang.org/doc/man/kernel_app.html
        ERL_AFLAGS = "-kernel shell_history enabled";
        ERL_INCLUDE_PATH = "${erlangLatest}/lib/erlang/usr/include";
        ERLANG_INTERFACE_PATH = "${erlangLibs.interface.path}";
        ERLANG_PATH = "${erlangLatest}";
      };
    in
      {

      apps = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          erlangLatest = pkgs.erlang_26;
          erlangLibs = getErlangLibs erlangLatest;
        in {});

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          erlangLatest = pkgs.erlang_26;
          erlangLibs = getErlangLibs erlangLatest;
        in
        {
          # `nix develop .#ci`
          # reduce the number of packages to the bare minimum needed for CI
          ci = pkgs.mkShell {
            env = mkEnvVars pkgs erlangLatest erlangLibs;
            buildInputs = with pkgs; [ erlangLatest just rebar3 ];
          };

          # `nix develop`
          default = pkgs.mkShell {
            packages = with pkgs; [
              erlang-ls
              erlfmt
              just
              rebar3
            ];
            shellHook = ''
              echo "Starting Erlang environment..."
              rebar3 get-deps
            '';
          };
        });
    };
}
