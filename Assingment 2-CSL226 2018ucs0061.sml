
(* Recursive insert sort program*)
fun insertSort R [] = []
	| insertSort R (h::t) =
	let 
	fun insert R [] x = [x]
		|   insert R (h::t) x = if R(x,h) then x::(h::t)
							else h::(insert R t x)
	val rest = insertSort R t 
	in insert R rest h
	end

 (* Tail recursive isnert sort program*)
fun insertSort_tr R arr = 

	let

	fun insert_tr R arr x=
   (* Appends Adds element in the last *)
		let
		fun append [] x = [x]
			|append (h::t) x = (h::t)@[x]
		(* accum acts as an acculator of sorted elements *)
		fun insert_acc R accum [] x =append accum x
			| insert_acc R accum (h::t) x = if R(x,h) then accum@(x::h::t)
											else insert_acc R (append accum h) t x

		in
		  insert_acc R [] arr x
		end

	fun insertSort_acc R acc [] = acc
	  | insertSort_acc R acc (h::t) = insertSort_acc R (insert_tr R acc h) t

	
	in
		insertSort_acc R [] arr
   
   end


(* The following sortcheck program takes a relation R , a sort program and an array to check if the sort program returns a sorted array *)	
(* Eg to check for insertSort_tr , call sortcheck op>= insertSort_tr [3,2,1]; *)
fun sortcheck R sort [] = true
	| sortcheck R sort arr =
	let	
    
	fun is_sorted R [] = true
        | is_sorted R (h::nil) = true
        | is_sorted R (h::t::nil) = if R(h,t) then true else false
        | is_sorted R (h::m::t) = if R(h,m) then is_sorted R (m::t) else false

    fun check_in x [] = false
        | check_in x (h::t)= if x=h then true else check_in x t

    fun remove x []=[]
        | remove x (h::t)= if x=h then t else h::(remove x t)

    fun is_permute [] [] = true
        | is_permute [] (h::t) =false
        | is_permute (h::t) [] =false
        | is_permute (h::t) (h1::t1)= if check_in h (h1::t1) then is_permute t (remove h (h1::t1)) else false

    val arr_1=sort R arr
    val arr_2=arr
    in
	if length arr_1 > length arr_2 orelse length arr_2 > length arr_1 then false
	else if is_sorted R arr_1 then is_permute arr_1 arr_2 else false
	end 
