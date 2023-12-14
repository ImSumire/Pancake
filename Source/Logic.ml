let anyEmpty (line : int array) : bool =
  Array.exists (fun x -> x = 0) line
;;

let newPiece () : piece =
  let i : int = Random.int(Array.length pieces) in
  { x = gw / 2 - 2; y = 0; shape = pieces.(i); color = colors.(i) }
;;

let newGrid () : mat =
  Array.make_matrix gh gw 0
;;

let inBounds (p : piece) : bool =
  let valid : bool ref = ref true in

  for y = 0 to Array.length p.shape - 1 do
    for x = 0 to Array.length p.shape.(0) - 1 do
      let rx = p.x + x in
      if p.shape.(y).(x) = 1 && (rx < 0 || rx >= gw) then
        valid := false
    done
  done;
  
  !valid
;;

let hasCollision (p : piece) : bool =
  (* Check if the piece is touching the floor or an other piece *)
  let collide : bool ref = ref false in
  for y = 0 to Array.length p.shape - 1 do
    for x = 0 to Array.length p.shape.(0) - 1 do
      if p.shape.(y).(x) = 1
        && (p.y + y >= gh || !grid.(p.y + y).(p.x + x) <> 0) then
          collide := true
    done
  done;
  !collide
;;

let directFall (p : piece) : unit =
  while not(hasCollision p) do
    p.y <- p.y + 1
  done;
  p.y <- p.y - 1
;;

let shiftLinesDown (start : int) : unit =
  for i = start downto 1 do
    !grid.(i) <- Array.copy !grid.(i - 1)
  done;
  !grid.(0) <- Array.make 10 0
;;

let handleLineClear () : unit =
  let n : int ref = ref 0 in
  for y = 0 to gh - 1 do
    if not (anyEmpty !grid.(y)) then (
      shiftLinesDown(y);
      n := !n + 1
    )
  done;
  score := !score + [|0; 100; 300; 500; 800|].(!n) * !level
;;

let addPiece (p : piece) : unit =
  for y = 0 to Array.length p.shape - 1 do
    for x = 0 to Array.length p.shape.(0) - 1 do
      if p.shape.(y).(x) = 1 then
        !grid.(p.y + y).(p.x + x) <- p.color
    done
  done
;;

let rotate (matrix, angle : mat * int) : mat =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let rotated = Array.make_matrix cols rows 0 in

  let rot i j =
    match angle with
    | 90 -> rotated.(j).(rows - 1 - i) <- matrix.(i).(j)
    | -90 -> rotated.(cols - 1 - j).(i) <- matrix.(i).(j)
    | _ -> failwith "Unsupported angle"
  in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      rot i j
    done
  done;

  rotated
;;
