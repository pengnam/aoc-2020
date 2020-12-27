open Core
type mask_instruction = 
  { mask:string
  }

type mem_instruction = 
  { location:int
  (* Change to large integer... but should be okay since we are running pon a 64bit os*)
  ; number: int
  }



type instruction = 
  Mask of mask_instruction
  | Memory of mem_instruction

let digits = 36
let bit_mask =(Int.pow 2 digits) - 1 

let empty_mask = 
  (* 0 mask *)
  bit_mask,
  (* 1 mask *)
  0,
  []

let one_at_pos integer pos =
  Int.bit_or integer (Int.shift_left 1 pos)

let zero_at_pos integer pos =
  Int.bit_and integer  (Int.bit_not (Int.shift_left 1 pos))


(* Note: For every mask, take the 'and' with the 0 mask and the 'or' with the 1 mask *)

let get_mask_for_mask_instruction mask =
  String.to_list mask
  |> 
  let string_length = String.length mask in 
  List.foldi
    ~f:(fun (index)(curr_mask)(char_at_index) ->
      let mask_0, mask_1, mask_x = curr_mask in 
      let bit_pos = string_length - index - 1 in 
      match char_at_index with
      'X' ->(mask_0, mask_1, bit_pos ::  mask_x)
      | '0' -> curr_mask
      | '1' -> mask_0, (one_at_pos mask_1 bit_pos), mask_x
      | _ -> raise (Failure "Yo not allowed")
    )
    ~init:empty_mask

  let bin_of_int d =
  if d < 0 then invalid_arg "bin_of_int" else
  if d = 0 then "0" else
  let rec aux acc d =
    if d = 0 then acc else
    aux (string_of_int (d land 1) :: acc) (d lsr 1)
  in
  String.concat ~sep:"" (aux [] d)

let apply_mask_on_value mask value = 
  let mask_0, mask_1, mask_x = mask in
  let value_before_x = value
      |> Int.bit_and mask_0
      |> Int.bit_or mask_1 in 
  let white_out_value = mask_x 
      |> List.fold 
      ~f: (fun (new_value)(x_pos) ->
        zero_at_pos new_value x_pos)
      ~init:value_before_x
  in
  Printf.printf "white out value:%s\n" (bin_of_int white_out_value);
  mask_x
  |> List.fold 
  ~f:(fun (candidate_list) (x_pos) ->
    candidate_list
      |> List.fold 
      ~f:(
        fun (new_candidate_list) (candidate) ->
          (one_at_pos candidate x_pos) :: new_candidate_list
      )
      ~init:candidate_list
  )
  ~init:[white_out_value;]

(* ============================================ *)

let empty_memory = Map.empty (module Int)

let add_memory keys value memory = 
  keys
      |> List.fold
      ~f: (fun (mem) (key) -> Map.set ~key: key ~data: value mem)
      ~init: memory




let get_result memory = 
  Map.data  memory
  |> List.fold
  ~f:(+)
  ~init:0


let string_of_list l =
  let rec aux acc l =
    match l with
    [] -> acc
      | h :: tl -> aux ((string_of_int h)::acc) (tl)
  in 
  String.concat ~sep:" " (aux [] l)

let print_contents memory =
  memory
      |> Map.to_alist  
      |> List.iter
        ~f:(fun (k,v) -> Printf.printf "%d: %s\n" k (bin_of_int v))



(* ============================================ *)

let parse_string_to_instruction raw_instruction = 
  String.split_on_chars ~on:[' '; '='] raw_instruction
  |> List.filter ~f:(fun segment-> (String.equal "" segment |> not ))
  |> fun (string_list) -> match (string_list) with 
    (indicator::value::[]) -> 
      if String.equal indicator "mask" then
        Mask {mask=value}
      else 
        let mem_reg = (Scanf.sscanf indicator "mem[%d]" (fun x->x)) in 
        Memory {
          location=  mem_reg;
          number= int_of_string value
        }
    |_ -> raise (Failure "Expected two elements")
let evaluate_instructions instructions =
  instructions
    |> List.fold 
    ~f:(
      fun (memory, mask) (instruction) -> 
        Printf.printf "--------------\n";
        (*print_contents memory;
        let mask_1, mask_2, mask_x = mask in 
        Printf.printf "Current mask %s & %s & %s || Current result: %d\n" (bin_of_int (mask_1)) (bin_of_int (mask_2)) (string_of_list mask_x) (get_result memory);*)
        match instruction with
        Mask instr -> (memory, get_mask_for_mask_instruction instr.mask)
        | Memory instr -> 
            let keys = apply_mask_on_value mask instr.location  in
            Printf.printf "keys: %s\n" (string_of_list keys);
            (add_memory keys instr.number memory), mask 
      )
    ~init: (empty_memory, empty_mask)

    


let part2 instructions = 
  let memory, _ = evaluate_instructions instructions in
    get_result memory

let instructions =
  CCIO.(with_in "./input" read_lines_l)
    |> List.map ~f:parse_string_to_instruction
    

    (*
    |> List.iter ~f:(fun instr ->
        (match instr with 
        Mask m -> (Printf.printf "mask instruction: %s" m.mask)
      |Memory m -> (Printf.printf "mem instruction: %d at %d" m.number m.location );

        )
    )
    *)





let _ = 
  instructions |> part2 |> (Printf.printf "part1: %d\n" ) ;;


