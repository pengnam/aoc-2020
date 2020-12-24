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
let print_seats seats =
  Array.iter 
  ~f:(fun row -> 
    Array.iter ~f:(fun s -> Printf.printf "%c" (Seat.to_char s)) row;
    Printf.printf "\n"
  )
  seats


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

let rec next_seating state_func m =
  print_seats m;
  Printf.printf "-------------\n";
  let dimx = Array.length m in
  let dimy = Array.length m.(0) in
  let new_seating = copy_matrix m in
  let modified = ref false in 
  for x = 0 to pred dimx do
    for y = 0 to pred dimy do
      (*Printf.printf "%d %d\n" x y;*)
      let next_s, modified_here = state_func m x y in 
        new_seating.(x).(y) <- next_s;
        if modified_here then modified := true;
    done
  done;
  if !modified then 
    next_seating state_func new_seating
  else
    new_seating
      
  

let parse_lines lines =
  let dimx = String.length (List.hd_exn lines)in
  let dimy = List.length lines in
  let data = Array.of_list lines in
  let grid = Array.make_matrix ~dimx:dimy ~dimy:dimx Seat.Empty in
  for x = 0 to pred dimx do
    for y = 0 to pred dimy do
      grid.(y).(x) <- Seat.of_char( String.get data.(y) x);
    done
  done;
  grid

let part1 grid = 
  next_seating next_state grid 


let rec get_occupied_in_dir g x y x_diff y_diff =
  let seat = 
      try g.(x).(y)
      with _ -> Seat.Empty in
    match seat
    with
    | Seat.Occupied -> 1
    | Seat.Floor -> get_occupied_in_dir g (x + x_diff) (y + y_diff) x_diff y_diff
    | _ -> 0


let count_surrounding_occupied_in_dir m x y =
  (get_occupied_in_dir m (x-1) (y-1) (0-1) (0-1)) +
  (get_occupied_in_dir m (x-1) (y  ) (0-1) (0  )) +
  (get_occupied_in_dir m (x-1) (y+1) (0-1) (0+1)) +
  (get_occupied_in_dir m (x  ) (y-1) (0  ) (0-1)) +
  (get_occupied_in_dir m (x  ) (y+1) (0  ) (0+1)) +
  (get_occupied_in_dir m (x+1) (y-1) (0+1) (0-1)) +
  (get_occupied_in_dir m (x+1) (y  ) (0+1) (0  )) +
  (get_occupied_in_dir m (x+1) (y+1) (0+1) (0+1)) 

let next_state_part_2 m x y = 
  let occupied_count = count_surrounding_occupied_in_dir m x y in
  match m.(x).(y), occupied_count with
  | Seat.Empty, 0 -> Seat.Occupied, true
  | Seat.Occupied, 5| Seat.Occupied, 6 | Seat.Occupied, 7| Seat.Occupied, 8 -> Seat.Empty, true
  | _ -> m.(x).(y), false

let part2 grid = 
  next_seating next_state_part_2 grid 

let seats =
  CCIO.(with_in "./input" read_lines_l)
  |> parse_lines


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
  seats |> part2 |> count_occupied |> Printf.printf "%d\n" ;;
