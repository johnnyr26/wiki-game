open! Core

let get_neighbors ~maze ~row ~col ~visited =
  let neighbors = ref [] in
  if row - 1 >= 0 then neighbors := (row - 1, col) :: !neighbors;
  if row + 1 < List.length maze
  then neighbors := (row + 1, col) :: !neighbors;
  if col - 1 >= 0 then neighbors := (row, col - 1) :: !neighbors;
  if col + 1 < List.length (List.nth_exn maze 0)
  then neighbors := (row, col + 1) :: !neighbors;
  List.filter !neighbors ~f:(fun (row, col) ->
    (not (List.exists visited ~f:(fun (r, c) -> row = r && col = c)))
    && (Char.equal (List.nth_exn (List.nth_exn maze row) col) '.'
        || Char.equal (List.nth_exn (List.nth_exn maze row) col) 'E'))
;;

let node_compare node1 node2 =
  let r1, c1 = node1 in
  let r2, c2 = node2 in
  r1 = r2 && c1 = c2
;;

let bfs ~maze ~row ~col =
  let visited = ref [] in
  let to_visit = ref [ row, col ] in
  let rec traverse () =
    if List.length !to_visit <> 0
    then (
      let current_node = List.hd_exn !to_visit in
      let row, col = current_node in
      to_visit := List.filteri !to_visit ~f:(fun i _ -> i <> 0);
      if Char.equal (List.nth_exn (List.nth_exn maze row) col) 'E'
      then (
        visited := current_node :: !visited;
        !visited
      )
      else (
        if not
             (List.exists !visited ~f:(fun node ->
                node_compare node current_node))
        then (
          visited := current_node :: !visited;
          let neighbors = get_neighbors ~maze ~row ~col ~visited:!visited in
          List.iter neighbors ~f:(fun next_node ->
            to_visit := next_node :: !to_visit));
        traverse ()))
    else !visited
  in
  traverse ()
;;

let parse_file ~(input_file : File_path.t) : char list list =
  In_channel.read_lines (File_path.to_string input_file)
  |> List.map ~f:(fun s -> String.to_list s)
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let maze = parse_file ~input_file in
        let visited = bfs ~maze ~row:1 ~col:0 in
        print_s [%message (visited : (int * int) list)]]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
