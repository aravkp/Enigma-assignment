(* breaker.ml - Enigma code breaker *)

let read_file path =
  let ic = open_in path in
  let rec read_lines acc =
    try let line = input_line ic in read_lines (line::acc)
    with End_of_file -> close_in ic; String.concat "\n" (List.rev acc)
  in
  read_lines []

(* Enigma logic, adapted for arbitrary settings *)
type config = {
  rotors: int list;
  positions: int list;
  ring_settings: int list;
  plugboard: (char * char) list;
}

let rotor_wiring = [
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ";  (* Rotor I *)
  "AJDKSIRUXBLHWTMCQGZNPYFVOE";  (* Rotor II *)
  "BDFHJLCPRTXVZNYEIWGAKMUSQO";  (* Rotor III *)
]
let reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

let apply_plugboard plugboard c =
  let rec find = function
    | [] -> c
    | (a,b)::tl -> if c = a then b else if c = b then a else find tl
  in
  find plugboard

let rotor_forward rotor_num pos ring_setting input =
  let wiring = List.nth rotor_wiring (rotor_num-1) in
  let adjusted = (input + pos - ring_setting + 26) mod 26 in
  let output_char = String.get wiring adjusted in
  let output = Char.code output_char - Char.code 'A' in
  (output - pos + ring_setting + 26) mod 26

let rotor_backward rotor_num pos ring_setting input =
  let wiring = List.nth rotor_wiring (rotor_num-1) in
  let adjusted = (input + pos - ring_setting + 26) mod 26 in
  let output_char = Char.chr (adjusted + Char.code 'A') in
  let output = String.index wiring output_char in
  (output - pos + ring_setting + 26) mod 26

let apply_reflector input =
  let output_char = String.get reflector input in
  Char.code output_char - Char.code 'A'

let notches = ['Q'; 'E'; 'V']

let rec advance_rotors rotors positions should_advance_next =
  match rotors, positions with
  | [r],[p] -> [r], [(p+1) mod 26]
  | r::rs, p::ps ->
      let notch_pos = Char.code (List.nth notches (r-1)) - Char.code 'A' in
      let should_advance = should_advance_next || (p = notch_pos) in
      let rs',ps' = advance_rotors rs ps should_advance in
      let new_p = if should_advance_next || should_advance then (p+1) mod 26 else p in
      r::rs', new_p::ps'
  | _ -> rotors, positions

let rec encrypt_chars rotors positions ring_settings plugboard chars acc =
  match chars with
  | [] -> String.concat "" (List.rev acc)
  | c::cs ->
    let new_rotors, new_positions = advance_rotors rotors positions false in
    let encrypted =
      if not (Char.uppercase_ascii c >= 'A' && Char.uppercase_ascii c <= 'Z') then c
      else
        let c_up = Char.uppercase_ascii c in
        let c_pb = apply_plugboard plugboard c_up in
        let input = Char.code c_pb - Char.code 'A' in
        let r1 = List.fold_left2 (fun acc rotor pos ->
          let ring = List.nth ring_settings (rotor-1) in
          rotor_forward rotor pos ring acc) input new_rotors new_positions in
        let r2 = apply_reflector r1 in
        let r3 = List.fold_left2 (fun acc rotor pos ->
          let ring = List.nth ring_settings (rotor-1) in
          rotor_backward rotor pos ring acc) r2 (List.rev new_rotors) (List.rev new_positions) in
        let out_c = Char.chr (r3 + Char.code 'A') in
        apply_plugboard plugboard out_c
    in
    encrypt_chars new_rotors new_positions ring_settings plugboard cs (String.make 1 encrypted :: acc)

let encrypt_string config text =
  let chars = List.init (String.length text) (String.get text) in
  encrypt_chars config.rotors config.positions config.ring_settings config.plugboard chars []
let decrypt_string = encrypt_string

(* Plugboard helpers: *)
let alphabet = Array.init 26 (fun i -> Char.chr (Char.code 'A' + i))
let all_single_swaps () =
  let rec aux i acc = if i >= 26 then acc
    else let rec loop j acc2 = if j >= 26 then acc2
      else if j > i then loop (j+1) ((alphabet.(i),alphabet.(j))::acc2) else loop (j+1) acc2
    in aux (i+1) (loop (i+1) acc)
  in aux 0 []
(* For two swaps, generate all non-overlapping pairs *)
let all_double_swaps () =
  let singles = all_single_swaps () in
  let rec disjoint (a,b) (c,d) =
    a <> c && a <> d && b <> c && b <> d
  in
  let rec build acc = function
    | [] -> acc
    | x::xs -> List.fold_left (fun acc2 y -> if disjoint x y then [x;y]::acc2 else acc2) (build acc xs) xs
  in build [] singles
let all_plugboards () =
  let singles = all_single_swaps () in
  let doubles = all_double_swaps () in
  [] :: (List.map (fun a -> [a]) singles) @ doubles

let contains_crib plaintext =
  let crib = "HEILHITLER" in
  let up = String.uppercase_ascii plaintext in
  try ignore (Str.search_forward (Str.regexp_string crib) up 0); true with Not_found -> false

let int_to_positions n = [n/(26*26); (n/26) mod 26; n mod 26]

let () =
  let ciphertext = read_file "output/sample.txt" in
  let ring_settings = [0;0;0] in
  let rotors = [1;2;3] in
  let plugboards = all_plugboards () in
  let found = ref false in
  let crib = "HEILHITLER" in
  for pos = 0 to 17575 do
    if pos mod 100 = 0 then Printf.printf "[INFO] Trying rotor position: %d / 17576 (%.1f%%)\n" pos (100.0 *. float_of_int pos /. 17576.0);
    let positions = int_to_positions pos in
    List.iter (fun plugboard ->
      if not !found then begin
        let config = {rotors; positions; ring_settings; plugboard} in
        let plaintext = decrypt_string config ciphertext in
        if (try let up = String.uppercase_ascii plaintext in ignore (Str.search_forward (Str.regexp_string crib) up 0); true with Not_found -> false)
        then begin
          Printf.printf "Found plausible config!\nPositions: %d %d %d\nPlugboard: %s\n---\n%s\n---\n"
            (List.nth positions 0) (List.nth positions 1) (List.nth positions 2)
            (String.concat ", " (List.map (fun (a,b) -> Printf.sprintf "%c<->%c" a b) plugboard))
            plaintext;
          found := true
        end
      end
    ) plugboards;
    if !found then exit 0
  done;
  if not !found then Printf.printf "No config produced desired crib in plaintext.\n"
