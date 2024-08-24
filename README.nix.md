Forester can also be used with `nix`. To run `forester`, use `nix run sourcehut:~jonsterling/ocaml-forester`. If you are working with a Nix flake-based project and want to include Forester as a build input, you can add it to your `flake.nix`:

```nix
{
  inputs = {
    forester.url = "sourcehut:~jonsterling/ocaml-forester";
    forester.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, forester, nixpkgs }:
    let
      system = "x86_64-linux"; # make sure to change this to your use case!
      pkgs = import nixpkgs { inherit system inputs; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [ forester.packages.${system}.default ];
      };
    };
}
```
