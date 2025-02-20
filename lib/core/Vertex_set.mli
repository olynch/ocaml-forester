type t

val empty : t
val add : Vertex.t -> t -> t
val diff : t -> t -> t

val of_list : Vertex.t list -> t
val to_list : t -> Vertex.t list
val to_seq : t -> Vertex.t Seq.t
val of_seq : Vertex.t Seq.t -> t
val union : t -> t -> t
val filter : (Vertex.t -> bool) -> t -> t

val elements : t -> Vertex.t list
