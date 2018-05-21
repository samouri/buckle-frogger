open Webapi.Dom;;

let appDiv = document |> Document.getElementById "app";;

(* ((map Element.setInnerHTML appDiv) "<h1>Buckle Down Baby</h1") *)

let sleep n = Js.Promise.make (fun ~resolve ~reject:_ -> 
    let _ : Js.Global.timeoutId =
      Js.Global.setTimeout
        (fun () -> (resolve "Done!")[@bs])
        n
    in ()
  );;

let sol1 = 
  let sum = ref 0 in 
  for i = 0 to 999 do 
    if i mod 3 = 0 || i mod 5 = 0 then sum := !sum + i;
    Js.log i;
  done;
  sum;;



Js.log sol1