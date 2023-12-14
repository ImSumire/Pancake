type clock = { mutable last : float; mutable interval : float };;

let newClock (rate : int) : clock =
  { last = Unix.gettimeofday(); interval = 1.0 /. float(rate) }
;;

let canTick (clk : clock) : bool =
  let now : float = Unix.gettimeofday() in
  let elapsed : float = now -. clk.last in
  if elapsed >= clk.interval then (
    clk.last <- now;
    true
  )
  else false
;;

let rec tick (clk : clock) : unit =
  let now : float = Unix.gettimeofday() in
  if canTick(clk) then
    clk.last <- now
  else
    tick(clk)
;;
