fun insertSort R [] = []
	| insertSort R (h::t) =
	let 
	fun insert R [] x = [x]
		|   insert R (h::t) x = if R(x,h) then x::(h::t)
							else h::(insert R t x)
	val rest = insertSort R t 
	in insert R rest h
	end

 


fun insertSort_tr R arr = 

	let

	fun insert_tr R arr x=
   (* Appends Adds element in the last *)
		let
		fun append [] x = [x]
			|append (h::t) x = (h::t)@[x]

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


fun sortcheck R sort [] = true
	| sortcheck R sort arr =
	let
	fun check_equal [] [] = true
		| check_equal[] (h::t) = false
		| check_equal (h::t) [] =false
		| check_equal (h1::t1) (h2::t2)= if h1=h2 then check_equal t1 t2 else false
	  val arr_1= sort R arr
	  val arr_2=insertSort R arr
	in
	if length arr_1 > length arr_2 orelse length arr_2 > length arr_1 then false
	else check_equal arr_1 arr_2 
	end
(* cons operation always gives result of form of element :: list of element 
Eg h::t will give t as a list of elements.
Simillarly, h::m::t is evaluated as h::(m::(t)). Therefore, h and m are elements but t is a list of elements.
The cons operator is right associative , ie a::b::c::d=(a::(b::(c::d))).

Also, the type of h::t is a list of type h.
*)

(* Here, the type of m is also a', but t is still list a' 
 fun append_2 (h::m::t) = m *)
