open Compiler
        
let () =
    let input = ref stdin in
    let output = ref stdout in
    let open Arg in
    parse
        [ "-in",String (fun s -> input := open_in s),"el nombre del fichero a compilar"
        ; "-out",String (fun s -> output := open_out s),"el nombre del fichero de salida" ]
        (fun arg -> raise (Bad("No se permiten argumentos anonimos")))
        "Compilador experimental de Arche";
    try
        compile_program ~input:!input ~output:!output;
        exit 0
    with e ->
        print_string (Printexc.to_string e);
        print_newline ();
        exit 1

