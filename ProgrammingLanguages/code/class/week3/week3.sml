
fun n_times (f, n, x) =
    if n = 0
    then x
    else f (n_times(f, n-1, x))

fun double x = x + x
fun increment x = x + 1

fun triple_n_times (n, x) = n_times((fn y => 3*y), n, x)
				   
fun filter (f, xs) =
    case xs of
	[] => []
      | x::xs' => if f x
		  then x::(filter(f,xs'))
		  else filter(f,xs')

fun allGreaterThan (xs, n) = filter (fn x => x > n, xs)

fun allShorterThan (xs, s) =
    let
	val i = String.size s
    in
	filter (fn x => String.size x < i, xs)
    end

fun fold f = fn acc => fn xs =>
		case xs of
		    [] => acc
		  | x::xs' => fold f (f(acc,x)) xs'

fun fold f acc xs =
    case xs of
	[] => acc
      | x::xs' => fold f (f(acc,x)) xs'
			   
fun exists predicate xs =
    case xs of
	[] => false
      | x::xs' => predicate x orelse exists predicate xs'

fun zip xs ys =
    case (xs, ys) of
	([],[]) => []
     |  (x::xs', y::ys') => (x,y)::(zip xs' ys')
     | _ => raise Empty
		  
fun range i j = if i > j then [] else i :: range (i+1) j

val countup = range 1

fun add_numbers xs = zip (countup (length xs)) xs
			 
fun other_curry1 f = fn x => fn y => f y x

fun other_curry2 f x y = f y x

fun curry f x y = f (x, y)

fun uncurry f (x, y) = f x y

val cbs : (int -> unit) list ref = ref []
fun onKeyEvent f = cbs := f::(!cbs)				       
fun onEvent i =
    let fun loop fs =
	    case fs of
		[] => ()
	      | f::fs' => (f i;loop fs')
    in
	loop (!cbs)
    end

val timesPressed = ref 0
val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1)
fun printIfPressed i =
    onKeyEvent (fn j => if i=j
			then print ("You pressed " ^ Int.toString i ^ "\n")
			else ())
	       
val empty_set =
    let
	fun make_set xs =
	    let
		fun contains i = List.exists (fn j => i=j) xs
	    in
		S {insert = fn i => if contains i
				    then make_set xs
				    else make_set(i::xs),
		   member = contains,
		   size   = fn () => length xs
		  }
	    end
    in
	make_set []
    end

