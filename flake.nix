{
  description = "Bluefin wrapper for log-base library.";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', inputs', pkgs, system, config, lib, ... }: {

        haskellProjects.default = {
          devShell = {
           enable = true;
           tools = hp: { fourmolu = hp.fourmolu; };

           hlsCheck.enable = true;
          };

          autoWire = [ "packages" "apps" "checks" ];
          settings.haskell-language-server.custom = with pkgs.haskell.lib.compose; lib.flip lib.pipe [
            (disableCabalFlag "ormolu")
          ];
        };
        packages.default = self'.packages.bluefin-log-base;

        devShells.default = pkgs.mkShell {
          name = "bluefin-log-base";
          meta.description = "bluefin-log-base dev shell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = with pkgs; [
            just
          ];
        };

      };
    };
}
