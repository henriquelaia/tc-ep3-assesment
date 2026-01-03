type estado = string [@@deriving yaml]

type simbolo = string

let simbolo_to_yaml s = `String s

let simbolo_of_yaml = function
  | `String s -> Ok s
  | `Float f -> Ok (string_of_int (int_of_float f))
  | `Bool b -> Ok (if b then "1" else "0")
  | v -> Error (`Msg ("Invalid simbolo in YAML: " ^ Yaml.to_string_exn v))

type direction = L | R | S

(* Custom serialization/deserialization for direction *)
let direction_to_yaml = function
  | L -> `String "L"
  | R -> `String "R"
  | S -> `String "S"

let direction_of_yaml = function
  | `String "L" -> Ok L
  | `String "R" -> Ok R
  | `String "S" -> Ok S
  | v -> Error (`Msg ("Invalid direction in YAML: " ^ Yaml.to_string_exn v))

type transition = estado * simbolo * direction [@@deriving yaml]

type delta = (string, (string, transition) Hashtbl.t) Hashtbl.t

(* Custom serialization/deserialization for delta *)
let delta_to_yaml delta =
  `O
    (Hashtbl.fold
       (fun key inner acc ->
         let inner_lst =
           Hashtbl.fold
             (fun k v acc2 -> (k, transition_to_yaml v) :: acc2)
             inner []
         in
         (key, `O inner_lst) :: acc )
       delta [] )

let delta_of_yaml = function
  | `O lst ->
      let outer_tbl = Hashtbl.create (List.length lst) in
      List.iter
        (fun (key, value) ->
          match value with
          | `O inner_lst ->
              let inner_tbl = Hashtbl.create (List.length inner_lst) in
              List.iter
                (fun (k, v) ->
                  let transition =
                    match transition_of_yaml v with
                    | Ok t -> t
                    | Error (`Msg m) -> failwith m
                  in
                  Hashtbl.add inner_tbl k transition )
                inner_lst ;
              Hashtbl.add outer_tbl key inner_tbl
          | v ->
              failwith ("Invalid inner delta YAML: " ^ Yaml.to_string_exn v) )
        lst ;
      Ok outer_tbl
  | v -> failwith ("Invalid delta YAML: " ^ Yaml.to_string_exn v)

type tm =
  { states: estado list
  ; input_alphabet: simbolo list
  ; tape_alphabet_extra: simbolo list
  ; start_state: estado
  ; accept_state: estado
  ; reject_state: estado
  ; delta: delta }
[@@deriving yaml]

type tm_w = {m: tm [@key "M"]; w: string} [@@deriving yaml]

let rec read_multiplelines () =
  try
    let line = read_line () in
    line ^ "\n" ^ read_multiplelines ()
  with End_of_file -> ""

let yaml_val = function
  | Ok v -> v (* v has type Yaml.value *)
  | Error (`Msg m) -> failwith m

(* string para tm_w *)
let tm_w_de_string s = s |> Yaml.of_string_exn |> tm_w_of_yaml |> yaml_val

(* tm_w para string *)
let tm_w_para_string tm_w = tm_w |> tm_w_to_yaml |> Yaml.to_string_exn

(* ------------------------------------------------------------ *)
(* EP3 — Simulação de TM de 1 fita (até 200 passos)              *)
(* Saída pedida no enunciado:                                    *)
(*   YES/NO + fita (terminando em _)                              *)
(*   DON'T KNOW (se > 200 passos)                                 *)
(*   INVALID (se input inválido)                                  *)
(*
   Nota: nos ficheiros de teste fornecidos por vezes aparece TRUE/FALSE.
   Se quiseres replicar exatamente esses testes locais, troca a função
   [string_of_bool_out] abaixo.
*)

let blank = "_"
let max_steps = 200

let string_of_bool_out (b : bool) = if b then "YES" else "NO"
(* alternativa para os testes antigos: if b then "TRUE" else "FALSE" *)

(* ---------- parsing seguro ---------- *)

let tm_w_de_string_safe (s : string) : tm_w option =
  try
    match Yaml.of_string s with
    | Error _ -> None
    | Ok y -> ( match tm_w_of_yaml y with Ok v -> Some v | Error _ -> None )
  with _ -> None

(* ---------- validação ---------- *)

let mem_string (x : string) (xs : string list) = List.exists (fun y -> x = y) xs

let uniq (xs : string list) : string list =
  let tbl = Hashtbl.create (List.length xs) in
  List.iter (fun x -> Hashtbl.replace tbl x ()) xs;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl []

let symbols_of_word (w : string) : simbolo list =
  (* Assunção do curso: cada símbolo é 1 char. *)
  List.init (String.length w) (fun i -> String.make 1 w.[i])

let validate_tm (m : tm) : bool =
  let states_ok =
    mem_string m.start_state m.states
    && mem_string m.accept_state m.states
    && mem_string m.reject_state m.states
  in
  let tape_alpha = uniq (m.input_alphabet @ m.tape_alphabet_extra) in
  let blank_ok = mem_string blank tape_alpha in
  let delta_ok =
    Hashtbl.fold
      (fun st inner acc ->
        acc
        && mem_string st m.states
        && Hashtbl.fold
             (fun read_sym (next_st, write_sym, _dir) acc2 ->
               acc2
               && mem_string read_sym tape_alpha
               && mem_string next_st m.states
               && mem_string write_sym tape_alpha )
             inner true )
      m.delta true
  in
  states_ok && blank_ok && delta_ok

let validate_input_word (m : tm) (w : string) : bool =
  let syms = symbols_of_word w in
  List.for_all (fun s -> mem_string s m.input_alphabet) syms

(* ---------- fita (zipper) ---------- *)

type tape = {left_rev: simbolo list; cur: simbolo; right: simbolo list}

let tape_of_word (w : string) : tape =
  match symbols_of_word w with
  | [] -> {left_rev = []; cur = blank; right = []}
  | x :: xs -> {left_rev = []; cur = x; right = xs}

let tape_read (t : tape) = t.cur

let tape_write (t : tape) (s : simbolo) = {t with cur = s}

let tape_move_right (t : tape) : tape =
  match t.right with
  | [] -> {left_rev = t.cur :: t.left_rev; cur = blank; right = []}
  | x :: xs -> {left_rev = t.cur :: t.left_rev; cur = x; right = xs}

let tape_move_left (t : tape) : tape =
  match t.left_rev with
  | [] -> {left_rev = []; cur = blank; right = t.cur :: t.right}
  | x :: xs -> {left_rev = xs; cur = x; right = t.cur :: t.right}

let tape_move (t : tape) = function
  | S -> t
  | R -> tape_move_right t
  | L -> tape_move_left t

let tape_to_symbols (t : tape) : simbolo list = List.rev t.left_rev @ (t.cur :: t.right)

let drop_while (p : 'a -> bool) (xs : 'a list) : 'a list =
  let rec go = function
    | [] -> []
    | x :: xs' -> if p x then go xs' else x :: xs'
  in
  go xs

let trim_blanks (xs : simbolo list) : simbolo list =
  let no_leading = drop_while (fun s -> s = blank) xs in
  let rev_no_trailing = drop_while (fun s -> s = blank) (List.rev no_leading) in
  List.rev rev_no_trailing

let tape_to_string (t : tape) : string =
  let core = tape_to_symbols t |> trim_blanks in
  match core with
  | [] -> blank
  | _ -> String.concat "" (core @ [blank])

(* ---------- simulação ---------- *)

type sim_result =
  | Accept of tape
  | Reject of tape
  | Dont_know

let lookup_transition (m : tm) (st : estado) (sym : simbolo) : transition option =
  match Hashtbl.find_opt m.delta st with
  | None -> None
  | Some inner -> Hashtbl.find_opt inner sym

let step (m : tm) (st : estado) (t : tape) : (estado * tape) option =
  let sym = tape_read t in
  match lookup_transition m st sym with
  | None -> None
  | Some (next_st, write_sym, dir) ->
      let t' = tape_write t write_sym |> fun t2 -> tape_move t2 dir in
      Some (next_st, t')

let simulate (m : tm) (w : string) : sim_result =
  let rec loop steps st t =
    if st = m.accept_state then Accept t
    else if st = m.reject_state then Reject t
    else if steps >= max_steps then Dont_know
    else
      match step m st t with
      | None -> Reject t (* máquina pára fora de qA/qR -> assumimos rejeição *)
      | Some (st', t') -> loop (steps + 1) st' t'
  in
  loop 0 m.start_state (tape_of_word w)

(* s1tm pedido no enunciado (só o resultado booleano) *)
let s1tm (m : tm) (w : string) : bool option =
  match simulate m w with
  | Accept _ -> Some true
  | Reject _ -> Some false
  | Dont_know -> None

  let () =
  ignore tm_w_de_string;
  ignore tm_w_para_string;
  ignore s1tm

(* ---------- programa ---------- *)

let () =
  let input = read_multiplelines () |> String.trim in
  match tm_w_de_string_safe input with
  | None ->
      print_endline "INVALID"
  | Some {m; w} ->
      if (not (validate_tm m)) || not (validate_input_word m w) then (
        print_endline "INVALID" )
      else
        match simulate m w with
        | Dont_know -> print_endline "DON'T KNOW"
        | Accept t ->
            print_endline (string_of_bool_out true) ;
            print_endline (tape_to_string t)
        | Reject t ->
            print_endline (string_of_bool_out false) ;
            print_endline (tape_to_string t)
