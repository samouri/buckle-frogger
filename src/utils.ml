open Types

let isSome = function 
  | Some _ -> true 
  | None -> false;;

let deoptionalize lst = 
  List.filter isSome lst
  |> List.map (function 
      | Some x -> x 
      | None -> assert false
    );;
(* define an infix operator to create a range between numbers. WTF this is crazy *)
let (<->) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j [] ;;

let intersects (rect1:rectT) (rect2:rectT) =
  let bottom1 = rect1.y +. (float_of_int rect1.height) in
  let bottom2 = rect2.y +. (float_of_int rect2.height) in
  let top1 = rect1.y in
  let top2 = rect2.y in
  let left1 = rect1.x in 
  let left2 = rect2.x in
  let right1 = rect1.x +. (float_of_int rect1.width) in 
  let right2 = rect2.x +. (float_of_int rect2.width ) in
  not ((bottom1 < top2 )|| 
       (top1 > bottom2) ||
       (right1 < left2) || 
       (left1 > right2));;


external spritesUrl: string = "../assets/frogger_sprites.png" [@@bs.module];;

let spriteSheet = Webapi.Dom.HtmlImageElement.make ();;
(Webapi.Dom.HtmlImageElement.src spriteSheet spritesUrl);;

let makeSpriteImage ?(number=1) ?(height=30) xStart yStart frames frameSpeed width = { 
  xStart; yStart; frames; frameSpeed; width; height; number;
};;

let yellowCarImage = makeSpriteImage 80 262 0 0. 33;;
let greenCarImage = makeSpriteImage 70 296 0 0. 33;;
let pinkCarImage = makeSpriteImage 10 262 0 0. 31;;
let raceCarImage = makeSpriteImage 40 260 0 0. 33;;
let whiteTruckImage = makeSpriteImage 110 296 0 0. 43;;
let threeTurtleImage = makeSpriteImage ~number:3 15 402 3 2. 35;;
let twoTurleImage = makeSpriteImage ~number:2 15 402 3 2. 35;;
let smallLogImage = makeSpriteImage 10 225 0 0. 80;;
let mediumLogImage = makeSpriteImage 10 193 0 0. 115;;
let bigLogImage = makeSpriteImage 10 162 0 0. 175;;
let frogUp = makeSpriteImage ~height:23 8 370 2 20. 28;;
let frogDown = makeSpriteImage ~height:23 76 370 2 20. 28;;
let frogLeft = makeSpriteImage ~height:23 76 335 2 20. 33;;
let frogRight = makeSpriteImage ~height:23 8 335 2 20. 33;;