open Core


let read_line file = match (In_channel.input_line file) with 
  | Some x -> x;
  | None -> raise (Failure "Expected something")

let read_input = 
  let file = In_channel.create "input" in 
    let timestamp = int_of_string (read_line file) in
    let buses = 
      String.split_on_chars (read_line file) ~on:[','] 
      |> List.filter ~f:(fun x -> (not (String.equal x "")) && (not (String.equal x "x"))) 
      |> List.map ~f:(int_of_string)
    in
    timestamp, buses
let modulo a n =
  let result = a mod n in
  if result >= 0  then 
    result
  else
    result + n

let read_input_2 = 
  let file = In_channel.create "input" in 
    let _ = int_of_string (read_line file) in
      String.split_on_chars (read_line file) ~on:[','] 
      |> List.filter ~f:(fun x -> (not (String.equal x ""))) 
      |> List.filter_mapi 
        ~f:(fun i x ->
          Printf.printf "%d %s\n" i x;
          try 
            let n = int_of_string x in Some (
              (modulo (n - i)  n), 
              n
              ) 
          with _ -> None
            )


let highest_multiple_greater_than lower_bound multiple = (((lower_bound + multiple - 1) / multiple) ) * multiple

let part1 (timestamp, buses) = 
  buses 
      |> List.fold 
        ~f:(fun (earliest_bus, earliest_time) (next_bus) ->
          let earliest_bus_for_next_bus = highest_multiple_greater_than timestamp next_bus in
            if  earliest_bus_for_next_bus < earliest_time then
              (next_bus, earliest_bus_for_next_bus) 
            else
              (earliest_bus, earliest_time)
            )
        ~init:(0,Int.max_value)
      |> fun (bus, time) -> bus * (time - timestamp)



let part2 (schedule) = 
  schedule 
  |> List.fold
    ~f:(fun (cumulative_remainder, cumulative_factor) (remainder, factor) ->
      Printf.printf "======= factor: %d %d ========\n" remainder factor;
      Out_channel.flush stdout;
      let new_remainder= ref cumulative_remainder in 
      while (Int.equal (!new_remainder mod factor) remainder |> not) do
        new_remainder := !new_remainder + cumulative_factor;
      done;
      Printf.printf "a-%d n-%d: %d %d\n" remainder factor !new_remainder (cumulative_factor*factor);
      (!new_remainder, cumulative_factor * factor)
    )
    ~init:(0, 1)





let _ = 
  read_input  |> part1 |> Printf.printf "%d\n";
  read_input_2  |> part2


