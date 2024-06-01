(** Another famous problem is this one: How can a knight jump on an N×N chessboard in such a way that it visits every square exactly once?

    Hint: Represent the squares by pairs of their coordinates (x,y), where both x and y are integers between 1 and N. Define the function jump n (x,y) that returns all coordinates (u,v) to which a knight can jump from (x,y) to on a n×n chessboard. And finally, represent the solution of our problem as a list knight positions (the knight's tour). *)

module IntPair = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    if Stdlib.compare x1 x2 = 0
    then if Stdlib.compare y1 y2 = 0 then 0 else Stdlib.compare y1 y2
    else Stdlib.compare x1 x2
  ;;
end

module IntPairSet = Set.Make (IntPair)

let directions = [ 2, -1; 2, 1; 1, 2; -1, 2; -2, 1; -2, -1; -1, -2; 1, -2 ]

let get_possible_moves (x : int) (y : int) : (int * int) list =
  let res = List.map (fun (x_dir, y_dir) -> x + x_dir, y + y_dir) directions in
  res
;;

let rec knights_tour_inner
  (n : int)
  ((x, y) : int * int)
  (tour : (int * int) list)
  (seen : IntPairSet.t)
  : (int * int) list
  =
  if List.length tour = n * n
  then (
    (* If we have seen all possible cells then stop *)
    let ans = List.rev tour in
    ans)
  else if x < 0 || x >= n || y < 0 || y >= n || IntPairSet.mem (x, y) seen
  then []
  else (
    let moves = get_possible_moves x y in
    let tour = (x, y) :: tour in
    let seen = IntPairSet.add (x, y) seen in
    let result = ref [] in
    List.iter
      (fun item ->
        (* If we already found a result skip the iteration for other items. *)
        if List.is_empty !result
        then (
          let res = knights_tour_inner n item tour seen in
          if List.is_empty res then () else result := res)
        else ())
      moves;
    !result)
;;

let knights_tour (n : int) ((x, y) : int * int) : (int * int) list =
  knights_tour_inner n (x, y) [] IntPairSet.empty
;;
