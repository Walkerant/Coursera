(* function 1 *)
fun only_capitals xs =
    List.filter (fn x => (Char.isUpper o (fn s => String.sub(s,0))) x) xs

(* function 2 *)
fun longest_string1 xs =
    List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" xs

(* function 3 *)
fun longest_string2 xs =
     List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" xs

(* function 4 *)
fun longest_string_helper f xs =
    List.foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" xs
	       
fun longest_string3 xs =
    longest_string_helper (fn (a,b) => a > b) xs
			  
fun longest_string4 xs =
    longest_string_helper (fn (a,b) => a >= b) xs	

(* function 5 *)
fun longest_capitalized xs =
    (longest_string1 o only_capitals) xs

(* function 6 *)
fun rev_string s =
    (String.implode o rev o String.explode) s

(* function 7 *)
exception NoAnswer
	      
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case (f x) of
		      SOME v => v
		    | NONE => first_answer f xs'

(* function 8 *)
fun all_answers f xs =
    let val some_answers = ((List.filter isSome) o (List.map f)) xs
	fun answer_list (ys, acc) =
	    case ys of
		[] => acc
	      | SOME y::ys' => answer_list (ys', y::acc) 
    in
	case (answer_list (some_answers, [])) of
	    [] => NONE
	  | lst => SOME lst
    end
	
(* function 9 *)	
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end	

(* function 9.a *)	
fun count_wildcards p =
    g (fn () => 1) (fn _ => 0) p

(* function 9.b *)
fun count_wild_and_variable_lengths p =
    g (fn () => 1) String.size p
    
(* function 9.c *)
fun count_some_var str_p =
    let val (str, p) = str_p
    in
	g (fn _ => 0) (fn s => if s=str then 1 else 0) p
    end
	
(* function 10 *)	
fun check_pat pat =
    let fun helper1 f1 f2 p =
	    let val r = helper1 f1 f2
	    in
		case p of
		    Wildcard          => f1 ()
		  | Variable x        => f2 x
		  | TupleP ps         => List.foldl (fn (p,i) => (r p) @ i) [] ps
		  | ConstructorP(_,p) => r p
		  | _                 => []
	    end
	fun helper2 xs =
	    case xs of
		[] => true
	      | x::[] => true
	      | x::xs' => if (List.exists (fn y => x=y) xs') then false else helper2 xs' 
    in
	(helper2 o (helper1 (fn _ => []) (fn x => [x]))) pat
    end
	
(* function 11 *)    
fun match (valu,pat) =
    case (valu,pat) of
	(_,Wildcard)    => SOME []
      | (_,Variable(s)) => SOME [(s,valu)]
      | (Unit,UnitP)    => SOME []
      | (Const i, ConstP j)    => if i=j then SOME [] else NONE
      | (Tuple(vs),TupleP(ps)) => if length vs = length ps
				  then all_answers match (ListPair.zip(vs,ps))
				  else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2
						   then match(v,p)
                                                   else NONE
      | _ => NONE			     		 

(* function 12 *)
fun first_match v ps =
    let val vp_pair_lst = List.map (fn p => (v,p)) ps
    in
	SOME (first_answer match vp_pair_lst) handle NoAnswer => NONE
    end	
			  
