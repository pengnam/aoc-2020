open Core


module Seat = struct
  type t = Occupied | Empty | Floor

  let of_char = function
    | 'L' -> Empty 
    | '.' -> Floor 
    | '#' -> Occupied
    | _ -> failwith "invalid Seat"
end


(* Consider adding types*)

let get_occupied g x y =
  try (if g.(x).(y) = Seat.Occupied then 1 else 0)
  with _ -> 0
 
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
  if m.(x).(y) = Seat.Empty && occupied_count = 0 then
    Seat.Occupied
  else if m.(x).(y) = Seat.Occupied && occupied_count >= 4 then
    Seat.Empty
  else
    m.(x).(y)

let copy_matrix m = Array.map Array.copy m

let rec next_seating m =
  let dimx = Array.length m.(0) in
  let dimy = Array.length m in
  let new_seating = copy_matrix m in
  let modified = ref False in 
  for x = 0 to pred dimx do
    for y = 0 to pred dimy do
      new_seating.(x).(y) = next_state m x y;
      if new_seating.(x).(y) != m.(x).(y) then
        modified = True;

    done
  done;
  if modified then 
    next_seating new_seating
  else
    new_seating
      
  

let parse_lines lines =
  let dimx = String.length (List.hd_exn lines)in
  let dimy = List.length lines in
  let data = Array.of_list lines in
  let grid = Array.make_matrix ~dimx:dimx ~dimy:dimy Seat.Empty in
  (* TODO: Use iteri *)
  for x = 0 to pred dimx do
    for y = 0 to pred dimy do
      grid.(x).(y) <- Seat.of_char( String.get data.(x) y)
    done
  done;
  Printf.printf "HI\n";
  grid

let part1 grid = 
  next_seating grid



let seats =
  CCIO.(with_in "./sample_input" read_lines_l)
  |> parse_lines





let () = 
  seats |> part1 |> (Printf.printf "%d\n");
