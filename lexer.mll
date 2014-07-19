{
    open Parser
}

let ws = [' ' '\t']

rule token = parse
    | ws+ {token lexbuf}
    | '\n'        {Lexing.new_line lexbuf;
                   token lexbuf}
    | "extend"   { EXTEND }
    | "end"      { END }
    | "with"     { WITH }
    | "new"      { NEW }
    | "function" { FUNCTION }
    | ":="        { IS }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "[" { LBRACKET }
    | "]" { RBRACKET}
    | "(" { LPAR }
    | ")" { RPAR }
    | ";" { SEMICOLON }
    | "," { COMMA }
    | "->" { ARROW }
    | "."  { DOT }
    | "+"  { PLUS }
    | "-"  { MINUS }
    | "*"  { MUL }
    | "/"  { DIV }
    | eof  { EOF } 
    | ['a'-'z''A'-'Z''_']+ as lxm { Id(lxm) }
    | ['0'-'9']+ as lxm { Int(int_of_string lxm) }
