fun quicksort [] = []
| quicksort (h::t)=
    let   
        fun partition [] L R pivot =(L,pivot,R)
            |partition (h::t) L R pivot =
                if h<=pivot then partition t (h::L) R pivot
                else partition t L (h::R) pivot

        val temp = partition t [] [] h
        val L_sort=quicksort(#1 temp)
        val R_sort=quicksort(#3 temp)
        val pivot= #2 temp
    in L_sort@[pivot]@R_sort
    end 

fun merge [] [] out = out
|   merge [] r out = out@r
|   merge l [] out = out@l
|   merge (left as h1::t1) (right as h2::t2) out =
    if h1<=h2 then merge t1 right (out@[h1])
    else merge left t2 (out@[h2]) 

(* In merge, we can also add element in front and then reverse the entire list in the end *)


fun split [] = ([],[])
|   split [a]= ([a],[])
|   split (h1::h2::t) =
    let val (left,right)=split t 
    in (h1::left,h2::right) 
    end
fun merge_sort []=[]
|   merge_sort [h]=[h] 
|   merge_sort arr=
    let val (left,right)=split arr
    val sorted_left=merge_sort(left)
    val sorted_right=merge_sort(right)
    in merge sorted_left sorted_right []
    end