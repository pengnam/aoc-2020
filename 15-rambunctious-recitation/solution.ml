open Core

module Counter = struct
  let empty = (Map.empty (module Int), 1)

  let size counter = 
    let counter_map, _ = counter in 
    (Map.length counter_map)

  let add_number number counter = 
    let counter_map, turn  = counter in
    let result =  Map.find counter_map number in
    match result with 
    Some prev_turn -> (
        ((Map.set ~key:number ~data:turn counter_map), turn + 1), (turn- prev_turn)
        )
    |None -> (
      ((Map.set ~key:number ~data:turn counter_map), turn + 1), 0
      )
  let initialize numbers = 
    numbers
    |> List.fold 
    ~f:(
      fun (counter)(number) -> 
        let new_counter, _ = add_number number counter in
        new_counter
    )
    ~init:empty
end


  


let read_numbers =
  let line = List.hd_exn (CCIO.(with_in "./sample_input" read_lines_l)) in 
  String.split_on_chars ~on:[','] line 
    |> List.map ~f:int_of_string 


let rec solve_helper counter number =
  if Int.equal (snd counter) 30000000 then
    number
  else
    let new_counter, next_number = Counter.add_number number counter in
    solve_helper new_counter next_number
    

let solve numbers =
  let counter = Counter.initialize numbers in
  solve_helper counter (0)




  





let _ = 
  read_numbers |> solve |> Printf.printf "%d\n" ;;


