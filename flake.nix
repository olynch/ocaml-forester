{
  nixConfig = {
    extra-substituters = [ "https://forester.cachix.org" ];
    extra-trusted-public-keys = [
      "forester.cachix.org-1:pErGVVci7kZWxxcbQ/To8Lvqp6nVTeyPf0efJxbrQDM="
    ];
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    opam-repository.url = "github:ocaml/opam-repository";
    opam-repository.flake = false;
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.opam-repository.follows = "opam-repository";
    };
  };
  outputs =
    {
      flake-utils,
      opam-nix,
      nixpkgs,
      ...
    }:
    let
      package = "forester";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        isGnu64 = system == "x86_64-linux";
        pkgsStatic =
          if isGnu64 then
            import nixpkgs {
              localSystem = nixpkgs.lib.systems.examples.gnu64;
              crossSystem = nixpkgs.lib.systems.examples.musl64;
            }
          else
            pkgs;
        on = opam-nix.lib.${system};
        devPackagesQuery = {
          ocaml-lsp-server = "*";
          odig = "*";
          alcotest = "*";
        };
        query = devPackagesQuery // {
          ocaml-system = "*";
        };
        mkScopes = pkgs: isStatic: rec {
          scope = on.buildDuneProject { inherit pkgs; } package ./. query;
          overlay = final: prev: {
            ${package} = prev.${package}.overrideAttrs (
              _:
              {
                doNixSupport = false;
              }
              // (
                if isStatic then
                  {
                    buildPhase = ''dune build -p ${package} --profile static -j $NIX_BUILD_CORES'';
                  }
                else
                  { }
              )
            );
            ocamlgraph = prev.ocamlgraph.overrideAttrs (_: {
              buildPhase = ''dune build -p ocamlgraph -j $NIX_BUILD_CORES'';
            });
            conf-gmp = prev.conf-gmp.overrideAttrs (_: {
              nativeBuildInputs = [ pkgs.pkgsBuildHost.stdenv.cc ];
            });
          };
          scope' = scope.overrideScope overlay;
          main = scope'.${package};
        };
        scopes = mkScopes pkgs false;
        scopesStatic = mkScopes pkgsStatic isGnu64;
        devPackages = builtins.attrValues (
          pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scopes.scope'
        );
      in
      {
        legacyPackages = scopes.scope';
        packages.default = scopesStatic.main;
        devShells.default = pkgs.mkShell {
          TOPIARY_LANGUAGE_DIR = "topiary";
          inputsFrom = [ scopes.main ];
          buildInputs =
            with pkgs;
            devPackages
            ++ [
              topiary
              reuse
              watchexec
            ];
        };
      }
    );
}
