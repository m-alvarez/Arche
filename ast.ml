type id = string
type op = Plus | Minus | Mul | Div
type literal =
    | Obj of (id * method_decl) list
    | Fn  of id list * expression
    | Int of int
and method_decl = 
    { self : id option
    ; pot  : int
    ; body : expression }
and expression =
    | Lit of literal
    | Name of id
    | New of expression
    | Extend of expression * expression
    | Apply of expression * expression list
    | Method of expression * id
    | Bin_op of op * expression * expression


