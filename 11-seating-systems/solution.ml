open Core


module Seat = struct
  type t = Occupied | Empty | Floor

  let of_char = function
    | 'L' -> Empty 
    | '.' -> Floor 
    | '#' -> Occupied
    | _ -> failwith "invalid Seat"

  let to_char s = match s with
  | Occupied -> '#'
  | Empty -> 'L'
  | Floor -> '.'
end


(* Consider adding types*)

let get_occupied g x y =
  let seat = 
      try g.(x).(y)
      with _ -> Seat.Empty in
    match seat
    with
    | Seat.Occupied -> 1
    | _ -> 0
let count_surrounding_occupied m x y =
  (get_occupied m (x-1) (y-1)) +
  (get_occupied m (x-1) (y  )) +
  (get_occupied m (x-1) (y+1)) +
  (get_occupied m (x  ) (y-1)) +
  (get_occupied m (x  ) (y+1)) +
  (get_occupied m (x+1) (y-1)) +
  (get_occupied m (x+1) (y  )) +
  (get_occupied m (x+1) (y+1)) 

let next_state m x y = 
  let occupied_count = count_surrounding_occupied m x y in
  match m.(x).(y), occupied_count with
  | Seat.Empty, 0 -> Seat.Occupied, true
  | Seat.Occupied, 4 | Seat.Occupied, 5| Seat.Occupied, 6 | Seat.Occupied, 7| Seat.Occupied, 8 -> Seat.Empty, true
  | _ -> m.(x).(y), false

let copy_matrix m = Array.map ~f:(Array.copy) m

let rec next_seating m =
  Printf.printf "Generating seating\n";
  let dimx = Array.length m in
  let dimy = Array.length m.(0) in
  let new_seating = copy_matrix m in
  let modified = ref false in 
  for x = 0 to pred dimx do
    for y = 0 to pred dimy do
      let next_s, modified_here = next_state m x y in 
        new_seating.(x).(y) <- next_s;
        if modified_here then modified := true;
    done
  done;
  if !modified then 
    next_seating new_seating
  else
    new_seating
      
  

let parse_lines lines =
  let dimx = String.length (List.hd_exn lines)in
  let dimy = List.length lines in
  let data = Array.of_list lines in
  let grid = Array.make_matrix ~dimx:dimy ~dimy:dimx Seat.Empty in
  Printf.printf "%d %d\n" dimx dimy;
  Printf.printf "%d %d\n" (Array.length grid) (Array.length grid.(0));
  (* TODO: Use iteri *)
  for x = 0 to pred dimx do
    for y = 0 to pred dimy do
      grid.(y).(x) <- Seat.of_char( String.get data.(y) x);
    done
  done;
  grid

let part1 grid = 
  next_seating grid



let seats =
  CCIO.(with_in "./input" read_lines_l)
  |> parse_lines

let print_seats seats =
  Array.iter 
  ~f:(fun row -> 
    Array.iter ~f:(fun s -> Printf.printf "%c" (Seat.to_char s)) row;
    Printf.printf "\n"
  )
  seats

let count_occupied m = 
  m
  |> Array.map
    ~f:(
        Array.fold 
        ~f:(
          fun (row_sum) (state) -> 
            match state with 
            | Seat.Occupied ->row_sum + 1
            | _ -> row_sum
          )
        ~init:(0)

    )
  |> Array.fold
       ~f:(+)
       ~init:(0)



let () = 
  seats |> part1 |> count_occupied |> Printf.printf "%d\n" ;;
