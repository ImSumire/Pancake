type mat = int array array;;
type piece = {
  mutable x: int;
  mutable y: int;
  mutable shape: mat;
  color: int 
};;