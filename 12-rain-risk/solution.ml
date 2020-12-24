

type position = {
  x:float;
  y:float;
}

type state = {
  angle: int;
  pos: position;
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
(*===============================*)

let n_transition state value = {
  angle = state.angle;
  pos = {
    x = state.pos.x;
    y = state.pos.y +. (float_of_int value);
  }
}

let s_transition state value = {
  angle = state.angle;
  pos = {
    x = state.pos.x;
    y = state.pos.y -. (float_of_int value);
  }
}
let e_transition state value = {
  angle = state.angle;
  pos = {
    x = state.pos.x +. (float_of_int value);
    y = state.pos.y;
  }
}
let w_transition state value = {
  angle = state.angle;
  pos = {
    x = state.pos.x -. (float_of_int value);
    y = state.pos.y;
  }
}
let l_transition state value = {
  angle = modulo (state.angle - value ) 360;
  pos = {
    x = state.pos.x;
    y = state.pos.y;
  }
}
let r_transition state value = {
  angle = modulo (state.angle + value) 360;
  pos = {
    x = state.pos.x;
    y = state.pos.y;
  }
}

let f_transition state value = {
  angle = state.angle;
  pos = {
    x = state.pos.x +. sin(to_radian state.angle) *. float_of_int value;
    y = state.pos.y +. cos(to_radian state.angle) *. float_of_int value;
  }
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
  angle = 90;
  pos = {
    x = 0.;
    y = 0.;
  }
}

let get_pos_from_state state = 
  state.pos

let manhattan_distance pos =
  (abs_float pos.x) +. (abs_float pos.y)


let next_state  state instruction m = 
    let transition = StringMap.find instruction.event m in
  let n_s = transition state instruction.value in
  n_s |>  fun s -> Printf.printf "%d %f %f %f\n"s.angle s.pos.x s.pos.y (manhattan_distance s.pos);
  n_s


let evaluate instructions =
    List.fold_left
    (fun (state) (instruction) -> next_state state instruction transition_map)
    initial_state
    instructions
  

(*===============================*)

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



