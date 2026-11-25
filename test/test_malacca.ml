(* End-to-end test runner.

   Usage: test_malacca.exe <path to compiler> <directory of .c programs>

   Every program in the directory is compiled with the compiler, executed, and
   checked against expectations that live in the program's own comments:

     // expect: some text      one line of expected stdout (repeat per line)
     // exit: N                expected exit status (default 0)
     // error: some text       compilation must FAIL and stderr must contain
                               this text (the program is not run)

   Keeping expectations next to the code means a test reads as a single file. *)

(* [lines_of_file path] reads a whole file into a list of lines. *)
let lines_of_file path =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  loop []

(* If [line] starts with [prefix], return the rest of it, else None.
   This is the usual OCaml way to say "maybe a value": [option]. *)
let strip_prefix prefix line =
  let pl = String.length prefix in
  if String.length line >= pl && String.sub line 0 pl = prefix then
    Some (String.sub line pl (String.length line - pl))
  else None

type expectation = {
  stdout_lines : string list;
  exit_code : int;
  compile_error : string option;
}

let parse_expectation path =
  let init = { stdout_lines = []; exit_code = 0; compile_error = None } in
  List.fold_left
    (fun e line ->
      match
        ( strip_prefix "// expect: " line,
          strip_prefix "// exit: " line,
          strip_prefix "// error: " line )
      with
      | Some text, _, _ -> { e with stdout_lines = e.stdout_lines @ [ text ] }
      | _, Some n, _ -> { e with exit_code = int_of_string (String.trim n) }
      | _, _, Some text -> { e with compile_error = Some text }
      | None, None, None -> e)
    init (lines_of_file path)

(* Run a shell command, returning (exit status, everything it printed on
   stdout). stderr is redirected into a file so it can be inspected too. *)
let run_capture cmd stderr_file =
  let full = Printf.sprintf "%s 2>%s" cmd (Filename.quote stderr_file) in
  let ic = Unix.open_process_in full in
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let status =
    match Unix.close_process_in ic with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n
  in
  (status, Buffer.contents buf)

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

(* Returns None on success or Some explanation on failure. *)
let run_test compiler tmp_dir path =
  let name = Filename.remove_extension (Filename.basename path) in
  let exe = Filename.concat tmp_dir name in
  let err_file = exe ^ ".stderr" in
  let expect = parse_expectation path in
  let cc_status, _ =
    run_capture
      (Filename.quote_command compiler [ "-o"; exe; path ])
      err_file
  in
  let cc_stderr = String.concat "\n" (lines_of_file err_file) in
  match expect.compile_error with
  | Some text ->
      if cc_status = 0 then Some "expected a compile error, but it compiled"
      else if not (contains ~sub:text cc_stderr) then
        Some (Printf.sprintf "error message %S does not contain %S" cc_stderr text)
      else None
  | None ->
      if cc_status <> 0 then
        Some (Printf.sprintf "compilation failed:\n%s" cc_stderr)
      else
        let status, out = run_capture (Filename.quote exe) err_file in
        let expected_out =
          String.concat "" (List.map (fun l -> l ^ "\n") expect.stdout_lines)
        in
        if out <> expected_out then
          Some (Printf.sprintf "stdout mismatch\n--- expected\n%s--- got\n%s"
                  expected_out out)
        else if status <> expect.exit_code then
          Some (Printf.sprintf "exit status %d, expected %d" status expect.exit_code)
        else None

let () =
  let compiler = Sys.argv.(1) and dir = Sys.argv.(2) in
  (* Absolute path: the tests chdir nowhere, but be safe for quote_command. *)
  let compiler =
    if Filename.is_relative compiler then Filename.concat (Sys.getcwd ()) compiler
    else compiler
  in
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ()) "malacca-tests" in
  (try Unix.mkdir tmp_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let programs =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".c")
    |> List.sort compare
  in
  let failures =
    List.filter
      (fun f ->
        match run_test compiler tmp_dir (Filename.concat dir f) with
        | None ->
            Printf.printf "ok    %s\n%!" f;
            false
        | Some why ->
            Printf.printf "FAIL  %s: %s\n%!" f why;
            true)
      programs
  in
  if failures <> [] then (
    Printf.printf "%d of %d tests failed\n" (List.length failures)
      (List.length programs);
    exit 1)
