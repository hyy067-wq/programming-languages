let countup_from1 (x:int) = 
  let rec count ((from:int)) = 
    if from = x then
      x :: []
    else
      from :: count (from+1)
  in
  count(1)
;;
