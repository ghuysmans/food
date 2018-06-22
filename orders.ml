let who () = Config.who

let db = Hashtbl.create 10

let fetch u =
  if Hashtbl.mem db u then
    Hashtbl.find db u
  else
    []

let update u s =
  Hashtbl.replace db u s
