val x = 1
val y = [2,3]
	    
fun countdown (n : int) =
    if n = 0
    then []
    else n::countdown(n-1)

		   
