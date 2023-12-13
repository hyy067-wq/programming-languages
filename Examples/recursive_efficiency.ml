let rec bad_max (xs : int list) =
  if xs = [] then
    0 
  else if (List.tl xs) = [] then
    List.hd xs
  else if List.hd xs > bad_max (List.tl xs) then
    List.hd xs
  else 
    bad_max (List.tl xs)
;;

let rec good_max (xs : int list) =
  if xs = [] then
    0
  else if (List.tl xs) = [] then
    List.hd xs
  else
    let tl_ans = good_max (List.tl xs) in
    if List.hd xs > tl_ans then
      List.hd xs
    else 
      tl_ans
;;
