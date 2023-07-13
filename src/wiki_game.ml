open! Core
open! Wikipedia_namespace
module Article = String

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href]"
  |> to_list
  |> List.map ~f:(fun a -> R.attribute "href" a)
  |> List.filter ~f:(fun a -> Option.is_none (namespace a) && (String.is_prefix a ~prefix: "/wiki/" || String.is_prefix a ~prefix: "https://en.wikipedia.org/wiki/"))
  |> List.sort ~compare: (fun a b -> String.compare a b)
  |> List.remove_consecutive_duplicates ~equal: (fun a b -> String.equal a b)
  |> List.map ~f: (fun link -> if not (String.is_prefix link ~prefix:"https://en.wikipedia.org") then "https://en.wikipedia.org" ^ link else link)
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

let rec dfs ~origin ~parent ~visited ~how_to_fetch ~depth ~max_depth : (string * string) list =
  let new_visited = visited @ [(parent, origin)] in 
  let article = File_fetcher.fetch_exn how_to_fetch ~resource: origin in 
  let linked_articles = get_linked_articles article in 
  if depth <> max_depth then 
  List.fold linked_articles ~init: new_visited ~f: (fun accu article -> 
    dfs ~origin: article ~parent: origin ~visited: accu ~how_to_fetch ~depth: (depth + 1) ~max_depth
  ) else new_visited


  
module G = Graph.Imperative.Graph.Concrete (Article)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
   include G
 
   (* These functions can be changed to tweak the appearance of the generated
      graph. Check out the ocamlgraph graphviz API
      (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
      for examples of what values can be set here. *)
   let edge_attributes _ = [ `Dir `None ]
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
   let vertex_name v = v
   let default_vertex_attributes _ = []
   let graph_attributes _ = []
 end)

let of_url s =
  String.split s ~on:'/'
  |> List.last_exn
  |> String.substr_replace_all ~pattern:"(" ~with_:""
  |> String.substr_replace_all ~pattern:")" ~with_:""
;;
 

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~(how_to_fetch: File_fetcher.How_to_fetch.t) () : unit =
  let graph = G.create () in
  dfs ~origin ~parent: origin ~visited: [] ~how_to_fetch ~depth: 0 ~max_depth
  |> List.filter ~f: (fun (parent, origin) -> not(String.equal parent origin))
  |> List.iter ~f:(fun (parent, origin) ->
    let parent_title = of_url parent in 
    let origin_title = of_url origin in 
    G.add_edge graph parent_title origin_title);
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph;
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let bfs ~origin ~destination ~how_to_fetch =
  let visited = ref [] in
  let to_visit = ref [ (origin, "") ] in
  let rec traverse () =
    if List.length !to_visit <> 0
    then (
      let (current_link, parent_link) = List.hd_exn !to_visit in
      print_s [%message (current_link: string)];
      let current_article = File_fetcher.fetch_exn how_to_fetch ~resource: current_link in 
      to_visit := List.filteri !to_visit ~f:(fun i _ -> i <> 0);
      if String.equal current_link destination
      then (
        visited := (current_link, parent_link):: !visited;
        !visited
      )
      else (
        if not
             (List.exists !visited ~f:(fun (link, _) -> String.equal current_link link))
        then (
          visited := (current_link, parent_link) :: !visited;
          let unfiltered_linked_articles = get_linked_articles current_article in
          let linked_articles = List.filter unfiltered_linked_articles ~f: (fun link -> List.exists !visited ~f: (fun (l, _) -> not(String.equal link l))) in
          List.iter linked_articles ~f:(fun linked_article ->
            to_visit := !to_visit @ [linked_article, current_link]));
        traverse ()))
    else !visited
  in
  traverse ()
;;

let rec backtrack explored destination origin path =
  match
    List.find_exn explored ~f:(fun (node, _) ->
      Article.equal node destination)
  with
  | node, parent ->
    if Article.equal node origin
    then [ node ] @ path
    else backtrack explored parent origin ([ node ] @ path)
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  let visited = bfs ~origin ~destination ~how_to_fetch in 
  let path = backtrack visited destination origin [] in
  ignore (max_depth : int);
  Some path;
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
