open Core




let create_jolt x = x

let parse_line line =
  Scanf.sscanf
    line
    "%d"
    create_jolt


let jolts =
  CCIO.(with_in "./input" read_lines_l)
  |> List.map ~f:parse_line


let rec diff = function
  | [] | [_]  -> []
  | x::(y::_ as t) -> (y-x) :: diff t


let count_elements x ls =
  List.count ls ~f:(fun y ->  x = y)

let part1 jolts =
  let differences = jolts
  |> List.append [0]
  |> List.sort ~compare:Int.compare
  |> diff
  in 
  count_elements 1 differences * (count_elements 3 differences + 1)


exception InvalidDifferenceBetweenConsecutiveElements of string

let part2 jolts =
  let _, _, ans, _ = jolts
    |> List.sort ~compare:Int.compare
    |> List.fold
      ~f:(fun (x, y, z, last_jolt) (jolt) ->
        let difference = jolt - last_jolt in
        if difference = 1 then (y, z, x + y + z, jolt)
        else if difference = 2 then (z, 0, y + z, jolt)
        else if difference = 3 then (0, 0, z, jolt)
        else raise ( InvalidDifferenceBetweenConsecutiveElements "oh no bb")
      )
      ~init:(0,0,1,0)
  in ans
  



let () = 
  jolts |> part1 |> (fun (x) -> Printf.printf "%d\n" x);
  jolts |> part2 |> (fun (x) -> Printf.printf "%d\n" x);
