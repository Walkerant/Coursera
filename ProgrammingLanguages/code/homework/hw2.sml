fun same_string (s1 : string, s2 : string) =
    s1 = s2

fun exist_in_list (s, sl) =
    case sl of
	[] => false
      | x::xs => if same_string(s,x) then true else exist_in_list(s,xs)

fun reverse lst =
    let fun aux(lst,acc) =
	    case lst of
		[] => acc
	      | x::xs => aux(xs, x::acc)
    in
	aux(lst,[])
    end
    
(* question 1.a *)
fun all_except_option (s, xs) =
    case xs of
	[] => NONE
      | x::xs' => if same_string(s,x)
		  then SOME xs'
		  else case all_except_option(s,xs') of
			   NONE => NONE
			 | SOME y => SOME(x::y)
					 

(* question 1.b *)
fun get_substitutions1 (sl, s) =
    case sl of
	[] => []
      | x::xs => case all_except_option(s,x) of
		     NONE => get_substitutions1(xs,s)
		   | SOME lst => lst @ get_substitutions1(xs,s)
								 
(* question 1.c *)
fun get_substitutions2 (sl, s) =
    let fun aux(sl, s, acc) =
	    case sl of
		[] => acc
	      | x::xs => case all_except_option(s,x) of
			     NONE => aux(xs,s,acc)
			   | SOME lst => aux(xs,s,(lst @ acc))
    in
	aux(sl, s, [])
    end

(* question 1.d *)
fun similar_names (sl, name) =
    let val {first = x,middle = y, last = z} = name
    in
	let val subs = get_substitutions1(sl,x)
	    fun aux (subs, acc) =
		case subs of
		    [] => acc
		  | xs::xs' => aux (xs', {first=xs,last=z,middle=y}::acc)
	in
	    {first=x,last=z,middle=y}::aux(reverse(subs),[])
	end
    end
	

	
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* function 2.a *)
fun card_color card =
    let val (s,_) = card
    in
	case s of
	    Spades => Black
	 |  Clubs => Black
	 | _ => Red
    end

(* function 2.b *)
fun card_value card =
    let val (_, r) = card
    in
	case r of
	    Num v => v
	  | Ace => 11
	  | _ => 10
    end

(* function 2.c *)
fun remove_card (cs, c, e) =
    let fun aux cs =
	    case cs of
		[] => raise e
	      | x::xs => if x = c then xs else x :: aux xs
    in
	aux cs
    end
	
	
(* function 2.d *)
fun all_same_color cards =
    case cards of
	[] => true
      | head::[] => true
      | head::neck::rest =>  (card_color(head)=card_color(neck)) andalso all_same_color(neck::rest)
										       
(* function 2.e *)
fun sum_cards cards =
    let fun aux(cards, acc) =
	    case cards of
		[] => acc
	      | x::xs => aux(xs, acc + card_value(x))
    in
	aux(cards, 0)
    end

(* function 2.f *)	
fun score (cards, goal) =
    let val sum = sum_cards(cards)
    in
	let fun get_pre_score (sum, goal) =
		if sum > goal
		then 3 * (sum - goal)
		else (goal - sum)
	in
	    if all_same_color(cards)
	    then get_pre_score(sum, goal) div 2
	    else get_pre_score(sum, goal)
	end
    end
	
(* function 2.g *)
fun officiate (cards,plays,goal) =
    let 
        fun loop (current_cards,cards_left,plays_left) =
            case plays_left of
                [] => score(current_cards,goal)
              | (Discard c)::tail => 
                loop (remove_card(current_cards,c,IllegalMove),cards_left,tail)
              | Draw::tail =>
                (* note: must score immediately if go over goal! *)
                case cards_left of
                    [] => score(current_cards,goal)
                  | c::rest => if sum_cards (c::current_cards) > goal
                               then score(c::current_cards,goal)
                               else loop (c::current_cards,rest,tail)
    in 
        loop ([],cards,plays)
    end
