open Relalg

let () =
  full_outer_join_u (["a", 1; "b", 2; "c", 3; "e", 8; "k", 9], ["a", 9; "d", 10]) |>
  List.iter (fun (n, r) ->
    match r with
    | Left l -> Printf.printf "%d€\t/\t%s\n" l n
    | Right r -> Printf.printf "/\t%d€\t%s\n" r n
    | Both (l, r) -> Printf.printf "%d€\t%d€\t%s\n" l r n
    (*
    | Some r -> Printf.printf "%d\t%d\t%s\n" l r n
    | None -> Printf.printf "%d\t/\t%s\n" l n
    *)
  )
