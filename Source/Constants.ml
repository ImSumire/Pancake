let pieces : mat array = [|
  [|
    [|0; 0; 0; 0|];
    [|1; 1; 1; 1|];
    [|0; 0; 0; 0|];
    [|0; 0; 0; 0|];
  |];

  [|
    [|1; 0; 0|];
    [|1; 1; 1|];
    [|0; 0; 0|];
  |];

  [|
    [|0; 0; 1|];
    [|1; 1; 1|];
    [|0; 0; 0|];
  |];

  [|
    [|1; 1|];
    [|1; 1|];
  |];

  [|
    [|0; 1; 1|];
    [|1; 1; 0|];
    [|0; 0; 0|];
  |];

  [|
    [|0; 1; 0|];
    [|1; 1; 1|];
    [|0; 0; 0|];
  |];

  [|
    [|1; 1; 0|];
    [|0; 1; 1|];
    [|0; 0; 0|];
  |];
|]
;;
let colors : int array = [|
  rgb 0    200  200;
  rgb 0    0    200;
  rgb 200  140  0;
  rgb 200  200  0;
  rgb 0    200  0;
  rgb 140  0    200;
  rgb 200  0    0
|]
let w : int = 450;;
let h : int = 720;;
let margin : int = 15;;
let gw : int = 10;;
let gh : int = 20;;
let size : int = 30;;