open Malacca
open Unix

let get_output_name input_file =
  match input_file with
  | Some f -> Filename.remove_extension f
  | None -> "a.out"

let write_to_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let run_command cmd =
  match Unix.create_process "/bin/sh" [|"/bin/sh"; "-c"; cmd|] Unix.stdin Unix.stdout Unix.stderr with
  | pid ->
      (match Unix.waitpid [] pid with
       | _, WEXITED 0 -> ()
       | _, WEXITED n ->
           Printf.eprintf "Command failed with exit code %d: %s\n" n cmd;
           exit n
       | _, WSIGNALED n ->
           Printf.eprintf "Command killed by signal %d: %s\n" n cmd;
           exit 1
       | _, WSTOPPED n ->
           Printf.eprintf "Command stopped by signal %d: %s\n" n cmd;
           exit 1)

let file_arg = try Some Sys.argv.(1) with _ -> None

let lexbuf =
  match file_arg with
  | Some file -> Lexing.from_channel (open_in file)
  | None -> Lexing.from_channel Stdlib.stdin

let print_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "line %d, column %d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let compile_and_link input_file =
  try
    let prog = Parse.main Lex.next_token lexbuf in
    let asm_code = Gen.program prog (Buffer.create 512) in

    let output_name = get_output_name input_file in
    let asm_file = output_name ^ ".s" in
    let obj_file = output_name ^ ".o" in

    write_to_file asm_file asm_code;

    (* Assemble *)
    run_command (Printf.sprintf "as -o %s %s" obj_file asm_file);

    (* Link *)
    run_command (Printf.sprintf "ld -o %s %s -lSystem -syslibroot $(xcrun -sdk macosx --show-sdk-path) -e _main -arch arm64" output_name obj_file);

    Sys.remove asm_file;
    Sys.remove obj_file

  with Parsing.Parse_error ->
    let pos = print_position lexbuf in
    Printf.eprintf "Parse error at %s\n" pos;
    exit 1

let _ = compile_and_link file_arg
