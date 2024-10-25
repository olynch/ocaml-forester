open Forester_core
module X = Forester_core.Types

let txt str = X.Text str

let p content = X.Prim (`P, X.Content content)
let ul content = X.Prim (`Ul, X.Content content)
let ol content = X.Prim (`Ol, X.Content content)
let li content = X.Prim (`Li, X.Content content)
let em content = X.Prim (`Em, X.Content content)
let strong content = X.Prim (`Strong, X.Content content)
let code content = X.Prim (`Code, X.Content content)
let blockquote content = X.Prim (`Blockquote, X.Content content)
let pre content = X.Prim (`Pre, X.Content content)
let figure content = X.Prim (`Figure, X.Content content)
let figcaption content = X.Prim (`Figcaption, X.Content content)
let cdata content = X.CDATA content
let contextual_number href = X.Contextual_number (Iri.of_string href)
let results_of_query query = X.Results_of_query query
let section content = X.Section content
let prim p content = X.Prim (p, content)
let katex m content = X.KaTeX (m, content)
let tex content = X.TeX_cs (Word content)
let img href = X.(Img (Remote href))
let content c = X.Content c

let xml_elt (prefix, uname) content =
  let open Forester_compiler in
  let prefix = Option.value ~default: "" prefix in
  let qname = X.{ prefix; uname; xmlns = None } in
  X.Xml_elt
    {
      name = qname;
      attrs = [];
      content = X.Content content
    }

let transclude href =
  X.Transclude
    X.{
      href = Iri.of_string href;
      target = Mainmatter;
      modifier = Identity
    }

let artefact content =
  X.Artefact
    X.{
      hash = "";
      content = Content content;
      sources = []
    }

let link href content =
  X.Link
    {
      href = Iri.of_string href;
      content = X.Content content;
    }
