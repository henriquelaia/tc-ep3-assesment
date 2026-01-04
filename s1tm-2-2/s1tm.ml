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

let () =
  ignore tm_w_de_string;
  ignore tm_w_para_string

(* EP3 - Simulador de TM (ate 200 passos)*)

let blank = "_"
let max_steps = 200

let fmt_bool (b : bool) = if b then "YES" else "NO"

(* ---------- parsing ---------- *)

let tm_w_de_string_safe (s : string) : tm_w option =
  try
    match Yaml.of_string s with
    | Error _ -> None
    | Ok y -> ( match tm_w_of_yaml y with Ok v -> Some v | Error _ -> None )
  with _ -> None

(* ---------- validacao ---------- *)

let contains_str (x : string) (xs : string list) = List.exists (fun y -> x = y) xs

let unique_items (xs : string list) : string list =
  let tbl = Hashtbl.create (List.length xs) in
  List.iter (fun x -> Hashtbl.replace tbl x ()) xs;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl []

let string_to_list (w : string) : simbolo list =
  (* Assume-se que cada simbolo e um caracter. *)
  List.init (String.length w) (fun i -> String.make 1 w.[i])

let check_tm_validity (m : tm) : bool =
  let states_ok =
    contains_str m.start_state m.states
    && contains_str m.accept_state m.states
    && contains_str m.reject_state m.states
  in
  let tape_alpha = unique_items (m.input_alphabet @ m.tape_alphabet_extra) in
  let blank_ok = contains_str blank tape_alpha in
  let delta_ok =
    Hashtbl.fold
      (fun st inner acc ->
        acc
        && contains_str st m.states
        && Hashtbl.fold
             (fun read_sym (next_st, write_sym, _dir) acc2 ->
               acc2
               && contains_str read_sym tape_alpha
               && contains_str next_st m.states
               && contains_str write_sym tape_alpha )
             inner true )
      m.delta true
  in
  states_ok && blank_ok && delta_ok

let check_word_validity (m : tm) (w : string) : bool =
  let syms = string_to_list w in
  List.for_all (fun s -> contains_str s m.input_alphabet) syms

(* ---------- fita ---------- *)

type zipper = {left_stack: simbolo list; current: simbolo; right_stack: simbolo list}

let init_zipper (w : string) : zipper =
  match string_to_list w with
  | [] -> {left_stack = []; current = blank; right_stack = []}
  | x :: xs -> {left_stack = []; current = x; right_stack = xs}

let read_head (t : zipper) = t.current

let write_head (t : zipper) (s : simbolo) = {t with current = s}

let move_head_right (t : zipper) : zipper =
  match t.right_stack with
  | [] -> {left_stack = t.current :: t.left_stack; current = blank; right_stack = []}
  | x :: xs -> {left_stack = t.current :: t.left_stack; current = x; right_stack = xs}

let move_head_left (t : zipper) : zipper =
  match t.left_stack with
  | [] -> {left_stack = []; current = blank; right_stack = t.current :: t.right_stack}
  | x :: xs -> {left_stack = xs; current = x; right_stack = t.current :: t.right_stack}

let move_head (t : zipper) = function
  | S -> t
  | R -> move_head_right t
  | L -> move_head_left t

let zipper_to_list (t : zipper) : simbolo list = List.rev t.left_stack @ (t.current :: t.right_stack)

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

let zipper_to_string (t : zipper) : string =
  let core = zipper_to_list t |> trim_blanks in
  match core with
  | [] -> blank
  | _ -> String.concat "" (core @ [blank])

(* ---------- simulacao ---------- *)

type run_outcome =
  | Accepted of zipper
  | Rejected of zipper
  | Undetermined

let lookup_transition (m : tm) (st : estado) (sym : simbolo) : transition option =
  match Hashtbl.find_opt m.delta st with
  | None -> None
  | Some inner -> Hashtbl.find_opt inner sym

let next_state (m : tm) (st : estado) (t : zipper) : (estado * zipper) option =
  let sym = read_head t in
  match lookup_transition m st sym with
  | None -> None
  | Some (next_st, write_sym, dir) ->
      let t' = write_head t write_sym |> fun t2 -> move_head t2 dir in
      Some (next_st, t')

let run_tm (m : tm) (w : string) : run_outcome =
  let rec loop steps st t =
    if st = m.accept_state then Accepted t
    else if st = m.reject_state then Rejected t
    else if steps >= max_steps then Undetermined
    else
      match next_state m st t with
      | None -> Rejected t (* Parou fora de qA ou qR, rejeita *)
      | Some (st', t') -> loop (steps + 1) st' t'
  in
  loop 0 m.start_state (init_zipper w)

(* s1tm (apenas booleano) *)
let s1tm (m : tm) (w : string) : bool option =
  match run_tm m w with
  | Accepted _ -> Some true
  | Rejected _ -> Some false
  | Undetermined -> None



(* ---------- programa ---------- *)

let () =
  let input = read_multiplelines () |> String.trim in
  match tm_w_de_string_safe input with
  | None ->
      print_endline "INVALID"
  | Some {m; w} ->
      if (not (check_tm_validity m)) || not (check_word_validity m w) then
        print_endline "INVALID"
      else
        match s1tm m w with
        | None ->
            print_endline "DON'T KNOW"
        | Some b ->
            (match run_tm m w with
             | Undetermined ->
                 print_endline "DON'T KNOW"
             | Accepted t ->
                 print_endline (fmt_bool b);
                 print_endline (zipper_to_string t)
             | Rejected t ->
                 print_endline (fmt_bool b);
                 print_endline (zipper_to_string t))
