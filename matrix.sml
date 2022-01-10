
(* There are no goddamn one element tuples in sml. Therefore,we can use brackets to clarify arguments in functions.*)

(* map can be used to apply a function to a list
eg. map f [] = [] and map f (h::t) = (f h)::(map f t) *)

fun power (x, 0) = 1  
| power (x, n) = x * power(x, n-1);

fun length [] = 0
| length (x::xs) = 1 + length xs

(* returns (bool,col,row) *)
fun dimension []= (true,0,0)
|   dimension [H]=(true,1,length(H))
|   dimension(H::TL)=
let
  val (b,r,c)=dimension(TL)
  val h = length H

in
    (b andalso(h=c),r+1,c)
end


 fun transpose [] = []  (*matrix is empty*)
 | transpose ([]::TL) = []    (*First row is empty (ie matrix made up of empty rows)*)
 | transpose LL = (map hd LL) :: (transpose (map tl LL))    
  
 (* map hd LL gives the first element of all rows ie first column *)
 (* and we recursively apply transpose on remainig matrix ie after removing first column*) 

(*Removes the ith row of the matrix*)

fun remove_row i []=[]
| remove_row 1 (H::TL)=TL
| remove_row i (H::TL)=H::remove_row (i-1) (TL)

(*Removes jth col of the matrix*)
fun remove_col j []=[]
| remove_col j LL=map (remove_row j) LL
 
(* fun cofactor i j matrix = power(-1,i+j)*det(remove_row(i (remove_col(j matrix))))

and

det []=0
| det ([]::tl)=0
| det   *)