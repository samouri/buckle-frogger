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
  direction: Direction.t;
};;
type spriteT = Car | BasicFloater | DivingTurtles;; 

type gameStateT = Start | Playing | Won | Lost;;

type spriteImageT = { xStart: int; yStart: int; frames: int; frameSpeed: float; width: int; height: int; number: int; };;

type laneObjectT = {
  rect: rectT;
  frameIndex: float;
  direction: Direction.t;
  img: spriteImageT;
  velocity: float;
  objType: spriteT;
}

type worldT = { 
  frog: frogT;
  input: Input.t;
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
