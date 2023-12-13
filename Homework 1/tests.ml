let test1 = is_older ((1, 2, 3), (2, 3, 4));;
let test2 = number_in_month ([(2012, 2, 28); (2013, 12, 1)], 2) = 1;;
let test3 = number_in_months ([(2012, 2, 28); (2011, 3, 31); (2011, 4, 28)], [2; 3; 4]) = 3
let test4 = dates_in_month ([(2012, 2, 28); (2013, 12, 1)], 2) = [(2012, 2, 28)]
let test5 = dates_in_months ([(2012, 2, 28); (2013, 12, 1); (2011, 3, 31); (2011, 4, 28)], [2; 3; 4]) = [(2012, 2, 28); (2011, 3, 31); (2011, 4, 28)] 
let test6 = get_nth (["hi"; "there"; "how"; "are"; "you"], 2) = "there"
let test7 = string_of_date (2013, 6, 1) = "June-1-2013"
let test8 = number_before_reaching_sum (10, [1;2;3;4;5]) = 3
let test9 = what_month 70 = 3
let test10 = month_range (31, 34) = [1;2;2;2]
let test11 = oldest ([(2012,2,28);(2011,3,31);(2011,4,28)]) = Some (2011,3,31)
let test12 = cumulative_sum [12;27;13] = [12;39;52]
