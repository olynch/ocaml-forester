open Base
module T := Xml_tree

type job = LaTeX_to_svg of {hash : string; source : string; content : svg:string -> T.content}
type result = {main : T.content T.article; side : T.content T.article list; jobs : job list}
val eval_tree : addr:addr -> source_path: string option -> Syn.tree -> result