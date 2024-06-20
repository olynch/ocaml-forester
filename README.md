This is the source repository for the [forester](https://sr.ht/~jonsterling/forester/) tool, which is implemented in the OCaml programming language. Please see [this page](https://www.jonmsterling.com/jms-005P.xml) for more information.

### System Requirements

You need to have [OCaml 5](https://ocaml.org) and [opam](https://opam.ocaml.org) installed.

### Installation

You can install forester by running `opam install forester`.

### Contributing

Please mail patches by [email](https://git-send-email.io/) to
<~jonsterling/forester-devel@lists.sr.ht>. General discussion can be mailed to
<~jonsterling/forester-discuss@lists.sr.ht>.

#### Using nix (Optional)

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

### Example Use

Please see my [Forest](https://github.com/jonsterling/forest) for an example of using forester, or clone your own [template forest](https://git.sr.ht/~jonsterling/forest-template).
