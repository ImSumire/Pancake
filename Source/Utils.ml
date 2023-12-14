let rgb (r : int) (g : int) (b : int) : int =
  if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then
    failwith "Invalid RGB components"
  else
    (r lsl 16) lor (g lsl 8) lor b
;;

let lighten (color, amount: int * int) : int =
  let r = (color lsr 16) land 255 in
  let g = (color lsr 8) land 255 in
  let b = color land 255 in

  let aux c =
    let nc = c + amount in
    if nc > 255 then 255 else nc
  in

  let nr = aux r in
  let ng = aux g in
  let nb = aux b in
  rgb nr ng nb
;;

let fillRect (x, y, w, h : int * int * int * int) : unit = 
  if Sys.os_type = "Unix" then  
    Graphics.fill_rect x y (w - 1) (h - 1)
  else
    Graphics.fill_rect x y w h
;;

let drawLine (x1, y1, x2, y2 : int * int * int * int) : unit =
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2
;;
