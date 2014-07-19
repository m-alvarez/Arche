open Ast
open Parser

module Context = Set.Make(String)

exception No_such_identifier of string

let compile_op = function
    | Plus  -> "+"
    | Minus -> "-"
    | Mul   -> "*"
    | Div   -> "/"

let rec compile context = function
    | Lit(l)  -> compile_literal context l
    | Name(n) -> 
            if Context.mem n context
            then n
            else raise (No_such_identifier n)
    | New(e) ->
            "instantiate(" ^ compile context e ^ ")"
    | Extend(e1, e2) ->
            "extend(" ^ compile context e1 ^ "," ^ compile context e2 ^ ")"
    | Apply(e1, exprs) ->
            let args = List.map (compile context) exprs in
            let arg_str = String.concat "," args in
            compile context e1 ^ "(" ^ arg_str ^ ")"
    | Method(e, i) ->
            let e = compile context e in
            "invoke(" ^ e ^ ", \"" ^ i ^ "\")"
    | Bin_op(op, left, right) ->
            let left = compile context left in
            let right = compile context right in
            left ^ compile_op op ^ right
and compile_literal context = function
    | Obj(methods) -> 
            let methods = List.map (compile_method context) methods in
            "{" ^ String.concat "," methods ^ "}"
    | Fn(args, body) ->
            let new_context = List.fold_right Context.add args context in
            Format.sprintf
            "function( %s ) {
                var result = %s;
                return result;
             }" (String.concat "," args) (compile new_context body)
    | Int(i) -> string_of_int i
and compile_method context (name, decl) =
    Format.sprintf "%s : %s" name (compile_method_body context decl)
and compile_method_body context decl =
    Format.sprintf "[%d, function() {
        %s
        return %s;
    }]" decl.pot 
        (match decl.self with
        | None -> ""
        | Some(self) -> "var " ^ self ^ "=this;")
        (compile (match decl.self with
                 | None -> context
                 | Some(self) -> Context.add self context) decl.body)
 
let add_prelude s =
    Format.sprintf
    "(function() {
        function invoke(obj, method) {
            var m = obj[method];
            var result;
            if (m[0] == 0) {
                return m[1].apply(obj);
            } else {
                throw Error(\"Method \" + method +
                    \" of object \" + obj + \" has potency \" + m[0]);
            }
        }
        function extend(obj1, obj2) {
            var object = {};
            for(method in obj1) {
                object[method] = obj1[method];
            }
            for(method in obj2) {
                object[method] = obj2[method];
            }

            return object;
        }
        function instantiate(obj) {
            var object = {};
            for(method in obj) {
                var potency = obj[method][0];
                var fn = obj[method][1];
                if(potency > 0) {
                    object[method] = [potency - 1, fn];
                }
            }
            object.class = obj;
            return object;
        }
        return %s
     })()" s

let compile_program ~input ~output =
    let compile_program expr = add_prelude (compile Context.empty expr) in
    let lexbuf = Lexing.from_channel input in
    let report message =
        let open Lexing in
        Format.printf "%s at line %d column %d\n"
            message
            lexbuf.lex_curr_p.pos_lnum
            (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
    in
    try
        let prog = Parser.pgm Lexer.token lexbuf in
        let compiled = compile_program prog in
        Printf.fprintf output "%s" compiled;
    with Parsing.Parse_error ->
        report "Parse error"
        | Failure(s) ->
        report s
        | e ->
        print_string (Printexc.to_string e);
        print_newline ();
        exit 1