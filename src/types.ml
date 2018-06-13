type directionT = Left | Right | Up | Down;;

type rectT = {
  x: float;
  y: float;
  width: int;
  height: int;
}

type frogT = {
  rect: rectT;
  leftInJump: float;
  leftInAnimation: int option;
  direction: directionT;
};;
type spriteT = Car | BasicFloater | DivingTurtles;; 

type gameStateT = Start | Playing | Won | Lost;;

type spriteImageT = { xStart: int; yStart: int; frames: int; frameSpeed: float; width: int; height: int; number: int; };;

type laneObjectT = {
  rect: rectT;
  frameIndex: float;
  direction: directionT;
  img: spriteImageT;
  velocity: float;
  objType: spriteT;
}

(* Represents the values of relevant key bindings. *)
type inputT = {
  mutable direction: directionT option;
  mutable bbox: bool;
  mutable grid: bool;
}

type worldT = { 
  frog: frogT;
  keys: inputT;
  objects: laneObjectT list;
  state: gameStateT;
  lives: int;
  score: int;
  highscore: int;
  maxRow: int;
  timer: int;
  endzone: (int * bool) list;
};;

type laneConfigT = {
  velocity: float;
  objectsAtOnceIsh: float;
  mutable nextSpawnTime: int;
  objType: spriteT;
  img: spriteImageT;
};;

type tempT = {
  laneCollisions: laneObjectT list;
  now: int;
}