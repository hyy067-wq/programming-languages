(* CSE 341, Homework 1, Provided Code *)

(* You might choose to uncomment these, like the lecture code does *)
(* #utop_prompt_dummy
   let _ = UTop.set_show_box false *)
(*
 * All inputs to the functions are "dates" of type (int * int * int)
 *)

(* Use these functions to extract parts of a date *)
let fst3 (x,_,_) = x (* gets the first element of a triple *)
let snd3 (_,x,_) = x (* gets the second element of a triple *)
let thd3 (_,_,x) = x (* gets the third element of a triple *)

(**
 * TODO: Complete the 12 function bindings described in the assignment.  For the first (2), 
 * we have given you the correct first line and an incorrect function body.
 *)

 (* 1 *)
let is_older ((date1 : int * int * int), (date2 : int * int * int)) =
  if (fst3 date1) <> (fst3 date2) then
    (fst3 date1) < (fst3 date2)
  else if (snd3 date1) <> (snd3 date2) then
    (snd3 date1) < (snd3 date2)
  else
    (thd3 date1) < (thd3 date2)
;;

(* 2 *)
let rec number_in_month ((dates : (int * int * int) list), (month : int)) =
  if dates = [] then
    0
  else
    if snd3 (List.hd dates) = month then
      1 + number_in_month (List.tl dates, month)
    else
      number_in_month (List.tl dates, month)
;;

(* continue for 3 and onward here *)
let rec number_in_months ((dates: (int * int * int) list), (months: int list)) =
  if months = [] || dates = [] then
    0
  else
    number_in_month (dates, (List.hd months)) + number_in_months (dates, (List.tl months))
;;

let rec dates_in_month ((dates : (int * int * int) list), (month: int)) = 
  if dates = [] then
    []
  else
    if (snd3 (List.hd dates) = month) then
      (List.hd dates) :: dates_in_month (List.tl dates, month)
    else
      dates_in_month (List.tl dates, month)
;;

let rec dates_in_months ((dates: (int * int * int) list), (months: int list)) =
  if months = [] || dates = [] then
    []
  else
    let ans = dates_in_month(dates, List.hd months) in
      if ans = [] then 
        []
      else
        ans @ dates_in_months(dates, List.tl months)
;;

let rec get_nth ((s : string list), (n : int)) = 
  if n = 1 then 
    List.hd s
  else
    get_nth(List.tl s, n - 1)
;;

let string_of_date (date : (int * int * int)) =
  let months = ["January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"] in
    get_nth(months, snd3 date) ^ "-" ^ string_of_int (thd3 date) ^ "-" ^ string_of_int (fst3 date)
;;

let number_before_reaching_sum ((sum : int), (numbers : int list)) =
  let rec helper ((list : int list), (current : int), (n : int)) =
    if current >= sum then
      n - 1
    else
      helper (List.tl list, current + List.hd list, n + 1)
  in 
  helper(numbers, 0, 0)
;;

let what_month ((day: int)) = 
  let month_durations = [31;28;31;30;31;30;31;31;30;31;30;31] in
    1 + number_before_reaching_sum (day, month_durations)
;;

let rec month_range ((day1: int), (day2: int)) = 
  if day1 > day2 then
    []
  else
    what_month day1 :: month_range(day1 + 1, day2)
;;

let oldest (dates: (int * int * int) list) = 
  if dates = [] then
    None
  else
    let rec oldest_nonempty (dates : (int * int * int) list) =
      if List.tl dates = [] then
        List.hd dates
      else
        let tl_ans = oldest_nonempty (List.tl dates) in
        if is_older (List.hd dates, tl_ans) then
          List.hd dates
        else
          tl_ans
    in
    Some (oldest_nonempty dates)
;;

let cumulative_sum (nums : int list) =
  let rec helper ((nums : int list), (sum : int)) =
    if nums = [] then
      []
    else
      (List.hd nums) + sum :: helper (List.tl nums, (List.hd nums) + sum)
  in
  helper (nums, 0)
;;
