open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and return a list
   of strings containing that actor's main credits. *)
let get_credits contents : string list =
  let open Soup in
  parse contents
  $$ "a[class]"
  |> to_list
  |> List.filter ~f:(fun a -> String.equal (R.attribute "class" a) "ipc-primary-image-list-card__title")
  |> List.map ~f:(fun a -> texts a |> String.concat ~sep:"" |> String.strip)
  |> List.sort ~compare: (fun a b -> String.compare a b)

;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"given an IMDB page for an actor, print out a list of their main credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group ~summary:"imdb commands" [ "print-credits", print_credits_command ]
;;


(*
   
<a class="ipc-primary-image-list-card__title" role="button" tabindex="0" aria-disabled="false" href="/title/tt6710474/?ref_=nm_knf_t_1">Everything Everywhere All at Once</a>*)