val days_of_months = [31,28,31,30,31,30,31,31,30,31,30,31]
val months_string = ["January","February","March","April","May","June","July","August","September","October","November","December"]

fun get_year (date : int*int*int) =
    (#1 date)

fun get_month (date : int*int*int) =
    (#2 date)

fun get_day (date : int*int*int) =
    (#3 date)

(* the number of days in the given month,eg Jan->31,Feb->28...*)
fun days_of_month (month : int) =
    let fun i_days_of_month (dms : int list, month : int) =
	    if month=1
	    then hd(dms)
	    else i_days_of_month(tl(dms), month-1)
    in
	i_days_of_month(days_of_months, month)
    end

(* the number of days of head months in one year *)
fun days_head_months (n : int) =
    let val days_current_month = days_of_month(n)
    in
	if n=1
	then days_current_month
	else days_current_month + days_head_months(n-1)
    end

(* the number of days in the that year, which ranges from 1 to 365 *)
fun days_of_year (date : int*int*int) =
    let val days_head_months = days_head_months(get_month(date)-1)
	val days = get_day(date)
    in
	(days_head_months + days)
    end

(* the total number of days the given date that starts by 1970-01-01 *)
fun days_from_1970 (date : int*int*int) =
    let val days_from_years = (get_year(date)-1970)*365
	val days_from_months = days_head_months(get_month(date)-1)
	val days = get_day(date)			  
    in
	(days_from_years + days_from_months + days)
    end

(* question 1 *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    let val days1 = days_from_1970(date1)
	val days2 = days_from_1970(date2)
    in
	days1 < days2
    end

(* question 2 *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else let val first = hd(dates)
	 in
	    if month=get_month(first)
	    then 1 + number_in_month(tl dates, month)
	    else number_in_month(tl dates, month)
	 end

(* question 3 *)
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else let val first_month = hd(months)
	 in
	     number_in_month(dates,first_month)+number_in_months(dates, tl(months))
	 end

(* question 4 *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
	let val first = hd(dates)
	in
	    if month=get_month(first)
	    then first::dates_in_month(tl(dates),month)
	    else dates_in_month(tl(dates),month)
	end

(* question 5 *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
    let fun append (xs : (int*int*int) list, ys : (int*int*int) list) =
	    if null xs
	    then ys
	    else hd(xs)::append(tl(xs), ys)
    in
	if null months
	then []
	else
	    let val first_month = hd(months)
	    in
		append(dates_in_month(dates,first_month),dates_in_months(dates,tl(months)))
	    end
    end

(* question 6 *)
fun get_nth (months : string list, n : int) =
    if n=1
    then hd(months)
    else get_nth(tl(months),n-1)
	
(* question 7 *)
fun date_to_string (date : int*int*int) =
    let val months_string = ["January","February","March","April","May","June","July","August","September","October","November","December"]
	fun month_to_string(months : string list, month : int) =
	    if month=1
	    then hd(months)
	    else month_to_string(tl(months),month-1)
    in
	let val month = month_to_string(months_string,get_month(date))
	    val year = Int.toString(get_year(date))
	    val day = Int.toString(get_day(date))
	in
	    ( month^" "^day^", "^year)
	end
    end
	
(* question 8 *)
fun number_before_reaching_sum (sum : int, xs : int list) =
    if sum <= hd xs
    then 0
    else 1 + number_before_reaching_sum(sum - hd xs, tl xs)
				   
(* question 9 *)
fun what_month (days : int) =
    1 + number_before_reaching_sum(days, days_of_months)

(* question 10 *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else let val first_month = what_month(day1)
	 in
	     first_month::month_range(day1+1,day2)
	 end

(* question 11 *)
fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else
	let fun the_oldest_nonempty (dates : (int*int*int) list) =
		if null (tl(dates))
		then hd dates
		else
		    let val ans_ton = the_oldest_nonempty(tl(dates))
		    in
			if is_older(hd(dates),ans_ton)
			then hd(dates)
			else ans_ton
		    end
	in
	    SOME (the_oldest_nonempty dates)
	end




					   
