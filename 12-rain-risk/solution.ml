

type position = {
  x:float;
  y:float;
}

type state = {
  pos_waypoint: position;
  pos_ship: position;
}

type transition_function = state-> int -> state

module StringMap = Map.Make(String)



type instruction = {
  event: string;
  value: int;
}


(*===============================*)
let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y




let pi = 4. *. atan 1.
let to_radian degree  = (float_of_int degree) *. pi /. 180.

let square y = y *. y


(*===============================*)
let shift_in_direction original_pos relative_pos magnitude = 
  let float_magnitude  = float_of_int magnitude in
  let new_original = 
    {
      x= original_pos.x +. (relative_pos.x -. original_pos.x) *. float_magnitude;
      y= original_pos.y +. (relative_pos.y -. original_pos.y) *. float_magnitude
    } in 
  let new_relative = {
    x= new_original.x +. (relative_pos.x -. original_pos.x);
    y= new_original.y +. (relative_pos.y -. original_pos.y)
  } in
  new_original, new_relative

let rotate_position center_position relative_position extra_degree = 
  let x_dist = relative_position.x -. center_position.x in
  let y_dist = relative_position.y -. center_position.y in
  let final_degree = to_radian extra_degree in 
  let sin_val = sin(final_degree) in
  let cos_val = cos(final_degree) in
      {
        x = center_position.x +. cos_val *. x_dist -. sin_val *. y_dist;
        y = center_position.y +. sin_val *. x_dist +. cos_val *. y_dist;
      }
        



let n_transition state value = {
  pos_ship = state.pos_ship;
  pos_waypoint = {
    x = state.pos_waypoint.x;
    y = state.pos_waypoint.y +. (float_of_int value);
  }
}

let s_transition state value = {
  pos_ship = state.pos_ship;
  pos_waypoint = {
    x = state.pos_waypoint.x;
    y = state.pos_waypoint.y -. (float_of_int value);
  }
}
let e_transition state value = {
  pos_ship = state.pos_ship;
  pos_waypoint = {
    x = state.pos_waypoint.x +. (float_of_int value);
    y = state.pos_waypoint.y;
  }
}
let w_transition state value = {
  pos_ship = state.pos_ship;
  pos_waypoint = {
    x = state.pos_waypoint.x -. (float_of_int value);
    y = state.pos_waypoint.y;
  }
}
let l_transition state value = {
  pos_ship = state.pos_ship;
  pos_waypoint = rotate_position state.pos_ship state.pos_waypoint (value)
}
let r_transition state value = {
  pos_ship = state.pos_ship;
  pos_waypoint = rotate_position state.pos_ship state.pos_waypoint ((-1) * value)
}

let f_transition state value =
  let new_pos_ship, new_pos_waypoint = shift_in_direction state.pos_ship state.pos_waypoint value in 
  {
    pos_ship = new_pos_ship;
    pos_waypoint = new_pos_waypoint
}


let transition_map = 
  StringMap.empty |> 
  StringMap.add "N" n_transition |>
  StringMap.add "S" s_transition |>
  StringMap.add "E" e_transition |>
  StringMap.add "W" w_transition |> 
  StringMap.add "L" l_transition |> 
  StringMap.add "R" r_transition |> 
  StringMap.add "F" f_transition

let initial_state = {
  (* Faces east *)
  pos_ship = {
    x = 0.;
    y = 0.;
  };
  pos_waypoint = {
    x = 10.;
    y = 1.;
  }
}


let manhattan_distance pos =
  (abs_float pos.x) +. (abs_float pos.y)


let next_state  state instruction m = 
    let transition = StringMap.find instruction.event m in
  let n_s = transition state instruction.value in
  n_s |>  fun s -> Printf.printf "ship: %f %f waypoint: %f %f distance: %f\n" s.pos_ship.x s.pos_ship.y s.pos_waypoint.x s.pos_waypoint.y (manhattan_distance s.pos_ship);
  n_s


let evaluate instructions =
    List.fold_left
    (fun (state) (instruction) -> next_state state instruction transition_map)
    initial_state
    instructions
  



(*==============================*)

let create_instruction c v = 
  {
    event = String.make 1 c;
    value = v;
  }

let parse_line line =
  Scanf.sscanf
    line
    "%c%d"
    create_instruction

let instructions =
  CCIO.(with_in "./input" read_lines_l)
  |> List.map parse_line



let _ = 
  instructions  |> evaluate  ;;
  (*|> List.map (fun instr -> Printf.printf "%s: %d\n" instr.event instr.value );;*)



