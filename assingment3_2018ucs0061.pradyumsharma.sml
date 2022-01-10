datatype 'a bintree = Empty | Node of 'a * 'a bintree * 'a bintree (* root,left,right*) (* Read the declaration as Node of type 'a *'a bintree * 'a bintree *)


(* Here we assume that R is like <=*)

(* Inserts a value into a bst*)
fun insert_bst_value  R tree value=
let
fun insert_bst R tree Empty = tree
|   insert_bst R Empty node = node
|   insert_bst R (Node(value,left,right)) (node as (Node(isrt_val,Empty,Empty)) ) =
if R(isrt_val,value) then (Node(value,(insert_bst R left node),right)) (*less than case*)
else (Node(value,left,(insert_bst R right node)))
in 
    insert_bst R tree (Node(value,Empty,Empty))
end

(*Converts a list to bst*)
fun list_to_bst R LL =
let
    fun list_to_bst_help R [] tree = tree
    | list_to_bst_help R (h::tl) tree = list_to_bst_help R tl (insert_bst_value R tree h ) (* Can also use (Node(h,Empty,Empty))*)
in
    list_to_bst_help R LL Empty
end


(*check_bst checks if a given bianry tree is a bst or not.*)
local
fun get_min (Node(value,Empty,_))=value
|   get_min (Node(value,left,_))=get_min(left)
fun get_max (Node(value,left,Empty))=value 
|   get_max (Node(value,left,right))=get_max(right)
in 
fun check_bst R Empty =true     (*Empty tree*)
|   check_bst R (Node(value,Empty,Empty))=true      (*Leaf*)
|   check_bst R (Node(value,left,Empty))=(R(get_max(left),value)) andalso (check_bst R left) (*Only left child*)
|   check_bst R (Node(value,Empty,right))=(R(value,get_min(right))) andalso (check_bst R right) (*Only right child*)
|   check_bst R (Node(value,left,right))=(R(get_max(left),value)) andalso (R(value,get_min(right))) andalso (check_bst R left) andalso (check_bst R right)  (*Both children*)
end

(*For delete_min,we donot need the relation as we know in bst,min is leftmost element*)
fun delete_min Empty = Empty  (*Empty tree*)
| delete_min (Node(value,Empty,Empty))=Empty (*Leaf*)
| delete_min (Node(value,Empty,right))=right (*Only right subtree*)
| delete_min (Node(value,left,right))= (Node(value,delete_min left,right)) 

(* Here we assume that the comparision relation is not reflexive*)
fun delete_node R Empty _ = Empty  (*Empty tree*)
|   delete_node R (tree as (Node(value,left,right))) del_val = 

(*get_min Returns min of NON-EMPTY bst*)
let
fun get_min (Node(value,Empty,_))=value
|   get_min (Node(value,left,_))=get_min(left)
fun get_max (Node(value,left,Empty))=value 
|   get_max (Node(value,left,right))=get_max(right)
fun isEmpty Empty=true
|   isEmpty _ =false
in
if del_val=value then (*Equality case*)
        if isEmpty(left) then right
        else if isEmpty(right) then left
        else Node(get_min right,left,(delete_min right))

else if  R(del_val,value) then Node(value,(delete_node R left del_val),right) (*value to be deleted is less*)
else  Node(value,left,(delete_node R right del_val))    (*value to be deleted is more*)

end

(*Example test case *)
(* val test = insert_bst_value (op<) (insert_bst_value (op<) Empty 1) 3; *)
(*  val test2=check_bst (op<) (delete_node (op<) (list_to_bst (op<)[5,1,3,2,4,7] ) 5);      
The above testcase converts the list [5,1,3,2,4,7] to bst, deletes 5 from it and check 
if the resulting tree is bst or not.
 *)