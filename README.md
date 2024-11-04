<!--
SPDX-FileCopyrightText: 2024 The Forester Project Contributors

SPDX-License-Identifier: GPL-3.0-or-later
-->

[![builds.sr.ht status](https://builds.sr.ht/~jonsterling/ocaml-forester.svg)](https://builds.sr.ht/~jonsterling/ocaml-forester?)

This is the source repository for the
[forester](https://sr.ht/~jonsterling/forester/) tool, which is implemented in
the OCaml programming language. Please see [this
page](https://www.forester-notes.org) for more information.


### System Requirements

You need to have [OCaml 5](https://ocaml.org) and
[opam](https://opam.ocaml.org) installed.

### Installation

You can install forester by running `opam install forester`. See
[README.nix.md](README.nix.md) for instructions for using Forester with `nix`.

### Contributing

Please mail patches by [email](https://git-send-email.io/) to
<~jonsterling/forester-devel@lists.sr.ht>. General discussion can be mailed to
<~jonsterling/forester-discuss@lists.sr.ht>.

When you prepare patches, please try to match the surrounding coding style to
the best of your ability (and do not use `ocamlformat`); patches will not be
rejected on grounds of poor formatting but they may be reformatted before being
applied. If you install [Topiary](https://topiary.tweag.io), you can format the
entire project using `./format.sh`.

[Join us on IRC](irc://irc.libera.chat/#forester)


### Example Use

Please see my [Forest](https://github.com/jonsterling/forest) for an example of using forester, or create your own forest using `forester init`.
