{
  description = "Canteen";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
  };

  outputs = { self, nixpkgs, devenv, ... } @ inputs:
    let
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = f: builtins.listToAttrs (map (name: { inherit name; value = f name; }) systems);
    in
      {
        packages = forAllSystems (system:
          let
            pkgs = nixpkgs.legacyPackages."${system}";
            poly = "${pkgs.polyml}/bin/polyc";
          in {
            canteen = pkgs.stdenv.mkDerivation {
              name = "canteen";
              src = ./.;
              installPhase = ''
                mkdir -p $out/bin
                ${poly} -o $out/bin/canteen build.sml
            '';};

          });

        apps = forAllSystems (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            poly = "${pkgs.polyml}/bin/polyc";
            mktemp = "${pkgs.coreutils}/bin/mktemp";
          in {
            build = {
              type = "app";
              program = toString (pkgs.writeShellScript "build-program" ''
              output=$(${mktemp})
              ${poly} -o $output build.sml && echo "Successfully built!"
            '');
            };
            execute = {
              type = "app";
              program = toString (pkgs.writeShellScript "build-program" ''
              output=$(${mktemp})
              ${poly} -o $output build.sml && $output
            '');
            };
          });

        devShells = forAllSystems (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
          in
            {
              default = devenv.lib.mkShell {
                inherit inputs pkgs;
                modules = [
                  ({ pkgs, ... }: {
                    packages = [
                      pkgs.gcc
                      pkgs.glibc
                      pkgs.just
                      pkgs.polyml
                      pkgs.mlton # required by smlfmt
                      pkgs.smlfmt];
                  })
                ];
              };
            });
      };
}
