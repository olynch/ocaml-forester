{
  nixConfig = {
    extra-substituters = [ "https://forester.cachix.org" ];
    extra-trusted-public-keys = [ "forester.cachix.org-1:pErGVVci7kZWxxcbQ/To8Lvqp6nVTeyPf0efJxbrQDM=" ];
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
      self,
      flake-utils,
      opam-nix,
      nixpkgs,
      opam-repository,
    }@inputs:
    let
      package = "forester";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgsDyn = pkgs;
        pkgsStatic = pkgs.pkgsStatic;
        on = opam-nix.lib.${system};
        devPackagesQuery = {
          ocaml-lsp-server = "*";
          ocamlformat = "*";
          memtrace = "*";
        };
        query = devPackagesQuery // {
          ocaml-system = "*";
        };
        mkScopes = pkgs: rec {
          isStatic = pkgs.stdenv.hostPlatform.isStatic;
          scope = on.buildDuneProject { inherit pkgs; } package ./. query;
          overlay = final: prev: {
            # You can add overrides here
            ${package} = prev.${package}.overrideAttrs (_: {
              doNixSupport = false;
            } // (if isStatic then {
              DUNE_PROFILE = "static";
            } else {}));
            ocamlgraph = prev.ocamlgraph.overrideAttrs (_: {
              buildPhase = ''dune build -p ocamlgraph -j $NIX_BUILD_CORES'';
            });
          };
          scope' = scope.overrideScope overlay;
          main = scope'.${package};
        };
        scopes = mkScopes pkgs;
        scopesStatic = mkScopes pkgsStatic;
        devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scopes.scope');
      in
      {
        legacyPackages = scopes.scope';
        packages.default = scopesStatic.main;
        devShells.default = pkgs.mkShell {
          TOPIARY_LANGUAGE_DIR = "topiary";
          inputsFrom = [ scopes.main ];
          buildInputs = devPackages ++ [
            pkgs.topiary
            pkgs.reuse
          ];
        };
      }
    );
}
