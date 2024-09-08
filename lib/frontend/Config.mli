module Forest_config: sig
  type t = {
    host: string option;
    root: string option;
    trees: string list;
    assets: string list;
    theme: string;
    stylesheet: string
  }
end

val default_forest_config : Forest_config.t
val parse_forest_config_file : string -> Forest_config.t
