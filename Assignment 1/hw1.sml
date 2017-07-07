fun is_older (date1 : int*int*int, date2 : int*int*int) =
  if (#1 date1) <> (#1 date2)
  then (#1 date1) < (#1 date2)
  else if (#2 date1) <> (#2 date2)
  then (#2 date1) < (#2 date2)
  else (#3 date1) < (#3 date2)
			
fun number_in_month(dates : (int*int*int) list, month : int) =
  if null dates
  then 0
  else
      let val tmp_ans = number_in_month(tl dates, month)
      in
	  if (#2 (hd dates)) = month
	  then 1 + tmp_ans
	  else tmp_ans
      end
	  
fun number_in_months(dates : (int*int*int) list, months : int list) =
  if null months
  then 0	   
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) =
  if null dates
  then []
  else
      let val tmp_ans = dates_in_month(tl dates, month)
      in
	  if (#2 (hd dates)) = month
	  then hd dates :: tmp_ans
	  else tmp_ans
      end

fun dates_in_months(dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
							 
fun get_nth(strs : string list, n : int) =
  if n = 1
  then hd strs
  else get_nth(tl strs, n - 1)

fun date_to_string(date : int*int*int) =
  let val month_names = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in get_nth(month_names,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum : int, nums : int list) =
  if sum <= hd nums
  then 0
  else 1 + number_before_reaching_sum(sum - hd nums, tl nums)
  
fun what_month(date : int) =
  let val month_days = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(date, month_days) + 1
  end

fun month_range(day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)
  
fun oldest (dates : (int*int*int) list) =
   if null dates
   then NONE
   else
       let
	   fun oldest_nonempty(dates : (int*int*int) list) =
	       if null (tl dates)
	       then hd dates
	       else
		   let val tmp_ans = oldest_nonempty(tl dates)
		   in
		       if is_older(hd dates, tmp_ans)
		       then hd dates
		       else tmp_ans
		   end
       in
	   SOME(oldest_nonempty(dates))
      end

fun mem (x : int, nums : int list) =
  if null nums
  then false
  else if x = hd nums
  then true
  else mem(x, tl nums)

fun remove_duplicates(nums : int list) =
  if null nums
  then []
  else
      let val ans = remove_duplicates(tl nums)
      in
	  if mem(hd nums,tl nums)
	  then ans
	  else hd nums :: ans
      end

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =
  number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
  dates_in_months(dates, remove_duplicates(months))

fun reasonable_date(date : (int*int*int)) =
  let
      val year = #1 date
      val month = #2 date
      val day = #3 date
      fun is_leap_year(year : int) =
	(year mod 400 = 0) orelse ((year mod 4 = 0) andalso  (year mod 100 <> 0))
      val feb_len = if is_leap_year(year) then 29 else 28
      val month_len = [31,feb_len,31,30,31,30,31,31,30,31,30,31]
      fun get_nth(nums : int list, n : int) =
	if n = 1
	then hd nums
	else get_nth(tl nums, n - 1)		    
  in
      year >= 1 andalso month >= 1 andalso month <= 12
      andalso day >= 1 andalso day <= get_nth(month_len, month)
  end
      
      
      
	       
	  
	  
