open Query

open Locally_nameless(Global_name)

let context vtx =
  rel Edges Incoming Rel.transclusion vtx

let backlinks vtx =
  rel Edges Incoming Rel.links vtx

let related vtx =
  isect
    [
      rel Edges Outgoing Rel.links vtx;
      complement @@ pred Pred.references
    ]

let contributions vtx =
  union
    [
      rel Edges Incoming Rel.authors vtx;
      rel Edges Incoming Rel.contributors vtx
    ]

let tree_under x =
  rel Paths Outgoing Rel.transclusion x

let references vtx =
  isect
    [
      union_fam_rel (tree_under vtx) Edges Outgoing Rel.links;
      pred Pred.references
    ]
