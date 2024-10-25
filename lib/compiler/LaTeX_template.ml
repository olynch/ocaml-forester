(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

let pp fmt ~preamble ~body =
  let newline () = Format.fprintf fmt "\n" in
  Format.fprintf fmt {|\documentclass[crop,dvisvgm]{standalone}|};
  newline ();
  Format.fprintf fmt "%s" preamble;
  newline ();
  Format.fprintf fmt {|\begin{document}|};
  newline ();
  Format.fprintf fmt "%s" body;
  newline ();
  Format.fprintf fmt {|\end{document}|}

let to_string ~preamble ~body =
  Format.asprintf "%a" (fun fmt _ -> pp ~preamble ~body fmt) ()
