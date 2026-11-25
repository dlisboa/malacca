(* Command-line driver: turns a C file into an executable.

     malacca prog.c            -> ./prog
     malacca prog.c -o out     -> ./out
     malacca -S prog.c         -> prog.s    (stop after generating assembly)
     malacca --dump-ast prog.c              (print the parsed tree and stop)

   The compiler proper (lib/) only produces assembly text. This file glues it
   to the system's assembler ([as]) and linker ([ld]). *)

open Malacca

let read_file path =
  let ic = open_in_bin path in
  let text = really_input_string ic (in_channel_length ic) in
  close_in ic;
  text

let write_file path text =
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc

(* Run [prog args...] (searched in PATH) and fail loudly if it doesn't exit 0.
   We pass an argv array rather than a shell string, so no quoting issues. *)
let run prog args =
  let argv = Array.of_list (prog :: args) in
  let pid = Unix.create_process prog argv Unix.stdin Unix.stdout Unix.stderr in
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _, _ -> raise (Compile.Error (prog ^ " failed"))

(* Where macOS keeps its system libraries/headers; the linker needs it to find
   libSystem (libc). [xcrun] is Apple's tool for locating developer files. *)
let sdk_path () =
  let ic = Unix.open_process_in "xcrun --sdk macosx --show-sdk-path" in
  let path = try input_line ic with End_of_file -> "" in
  ignore (Unix.close_process_in ic);
  if path = "" then raise (Compile.Error "cannot locate the macOS SDK (xcrun)");
  path

let assemble_and_link ~asm ~output =
  let asm_file = Filename.temp_file "malacca" ".s" in
  let obj_file = Filename.temp_file "malacca" ".o" in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove asm_file with Sys_error _ -> ());
      try Sys.remove obj_file with Sys_error _ -> ())
    (fun () ->
      write_file asm_file asm;
      run "as" [ "-arch"; "arm64"; "-o"; obj_file; asm_file ];
      (* -e _main: start at main. dyld then calls it and passes its return
         value to exit(), so we don't need the usual crt0 startup file. *)
      run "ld"
        [ "-arch"; "arm64"; "-o"; output; obj_file; "-lSystem";
          "-syslibroot"; sdk_path (); "-e"; "_main" ])

let usage = "usage: malacca [-S] [--dump-ast] [-o output] file.c"

let () =
  let input = ref None
  and output = ref None
  and asm_only = ref false
  and dump_ast = ref false in
  Arg.parse
    [ ("-o", Arg.String (fun s -> output := Some s), "<file> write output to <file>");
      ("-S", Arg.Set asm_only, " stop after generating assembly");
      ("--dump-ast", Arg.Set dump_ast, " print the syntax tree and stop") ]
    (fun f -> input := Some f)
    usage;
  match !input with
  | None ->
      prerr_endline usage;
      exit 2
  | Some input -> (
      try
        let program = Compile.parse ~filename:input (read_file input) in
        if !dump_ast then print_endline (Ast.show_program program)
        else
          let asm = Compile.to_assembly program in
          let base = Filename.remove_extension input in
          if !asm_only then
            write_file (Option.value !output ~default:(base ^ ".s")) asm
          else
            assemble_and_link ~asm ~output:(Option.value !output ~default:base)
      with
      | Compile.Error msg ->
          prerr_endline msg;
          exit 1
      | Sys_error msg ->
          prerr_endline ("malacca: " ^ msg);
          exit 1)
