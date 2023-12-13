let rec better_max (xs : int list) = 
  if xs = [] then
    None
  else
    let tl_ans = better_max (List.tl xs) in
    if tl_ans = None then
      Some (List.hd xs)
    else if Option.get tl_ans <= List.hd xs then
      Some (List.hd xs)
    else
      tl_ans
;;

let better_max2 (xs : int list) =
  if xs = [] then 
    None
  else 
    let rec max_nonempty (xs : int list) = (* fine to assume nonempty here *)
      if (List.tl xs) = [] then
        List.hd xs
      else
        let tl_ans = max_nonempty (List.tl xs) in
          if List.hd xs > tl_ans then
            List.hd xs
          else
            tl_ans
    in
    Some (max_nonempty xs)
;;
