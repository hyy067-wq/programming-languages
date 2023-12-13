let rec prod_list (xs : int list) = 
  if xs = [] then
    1
  else
    List.hd xs * prod_list(List.tl xs)
;;

let x = 4;;
