(*
  How to install Graphics ?
  $ opam install graphics

  How to run this code ?
  $ ocaml Main.ml
*)

(*
  Code tested with:
  - Windows 10
  - Ubuntu 23.04
*)

#use "topfind";;
#require "graphics";;
#require "unix";;


#use "Source/Types.ml";;
#use "Source/Utils.ml";;
#use "Source/Constants.ml";;
#use "Source/Clock.ml";;

(* Vars *)
let score : int ref = ref 0;;
let level : int ref = ref 1;;
let grid : mat ref = ref([|[||]|]);;

#use "Source/Logic.ml";;

let floating : piece ref = ref (newPiece());;

(* Game *)
let openApp (w, h : int * int) : unit = 
  if Sys.os_type = "Unix" then  
    let s = ":0 " ^ string_of_int(w) ^ "x" ^ string_of_int(h) in
      Graphics.open_graph s
  else
    let s = string_of_int(w) ^ "x" ^ string_of_int(h) in
      Graphics.open_graph s
;;

let init () : unit =
  (* Graphic *)
  openApp(w, h);
  Graphics.set_window_title "Tetris 95";

  (* Logic *)
  Random.self_init();
  grid := newGrid();
;;

let clear () : unit =
  Graphics.set_color(rgb 40 40 40);
  fillRect(0, 0, w, h)
;;

let drawBox (x, y, c : int * int * int) : unit =
  Graphics.set_color(c);
  let rx: int = x * size + margin in
  let ry: int = (gh - 1 - y) * size + margin in
  fillRect(rx, ry, size, size)
;;

let drawFloating () : unit =
  for y = 0 to Array.length !floating.shape - 1 do
    for x = 0 to Array.length !floating.shape.(0) - 1 do
      if !floating.shape.(y).(x) = 1 then
        drawBox(!floating.x + x, !floating.y + y, !floating.color)
    done
  done
;;

let drawGrid () : unit =
  (* Frame *)
  Graphics.set_color(rgb 80 80 80);
  fillRect(margin, margin, gw * size, gh * size);

  (* Floating piece *)
  drawFloating();

  (* Pieces *)
  for y = 0 to gh - 1 do
    for x = 0 to gw - 1 do
      if !grid.(y).(x) <> 0 then
        drawBox(x, y, !grid.(y).(x))
    done
  done;

  (* Lines *)
  Graphics.set_color(rgb 40 40 40);
  for y = 0 to gh - 1 do
    drawLine(
      margin,
      margin + y * size,
      margin + gw * size,
      margin + y * size
    )
  done;

  for x = 0 to gw - 1 do
    drawLine(
      margin + x * size,
      margin,
      margin + x * size,
      margin + gh * size
    )
  done
;;

let handleInput (p : piece) : unit =
  if Graphics.key_pressed() then
    match Graphics.read_key() with
    | 'q' -> p.x <- p.x - 1
    | 'd' -> p.x <- p.x + 1
    | 's' -> p.y <- p.y + 1
    | ' ' -> directFall(p)
    | 'z' -> p.shape <- rotate(p.shape, 90)
    | 'a' -> p.shape <- rotate(p.shape, -90)
    | _ -> ()
;;

let loop () : unit =
  let clk : clock = newClock(60) in
  let updateClk : clock = newClock(1) in
  while true do
    (* Update *)
    let next : piece = match !floating with
    | { x; y; shape; color } -> { x; y; shape; color } in
    (
      if canTick(updateClk) then
        next.y <- next.y + 1
      else
        (* Prevent double movement *)
        handleInput(next);
      
      (* Prevent out of bounds *)
      if inBounds(next) then (
        if hasCollision(next) then (
          addPiece(!floating);
          floating := newPiece()
        )
        else (
          !floating.x <- next.x;
          !floating.y <- next.y;
          !floating.shape <- next.shape
        )
      )
    );
    handleLineClear();

    (* Render *)
    clear();
    drawGrid();
    
    tick(clk)
  done
;;

init();;
loop();;
