structure tree:tree =
struct

datatype 'a tree = Empty | Node of 'a * 'a clump
and 'a clump = Cons of 'a tree * 'a clump | Nil

(* preorder_clump takes a list of 'a and adds preorder of all trees in that clump*)
fun preorder Empty= []
|   preorder tr =
let
fun preorder_clump Nil ll =ll
|   preorder_clump (Cons(tr,clmp)) ll = preorder_clump clmp (ll@(preorder_tree(tr)))
and  
(* Takes a tree and returns a list of all elements of the list in preorder*)
preorder_tree Empty = []
|   preorder_tree (Node(node_val,cl))= node_val::(preorder_clump cl [])
in preorder_tree tr
end


fun postorder Empty = []
|   postorder tr =
let
fun postorder_clump Nil ll=ll
|   postorder_clump (Cons(tr,clmp)) ll = postorder_clump clmp (ll@(postorder_tree(tr)))
and postorder_tree Empty =[]
|   postorder_tree (Node(node_val,cl))=(postorder_clump cl []) @[node_val]
in postorder_tree tr
end

fun map f tr =
let
fun map_tree f Empty = Empty
|   map_tree f (Node(node_val,clmp))= Node(f(node_val), map_clump f clmp)
and 
map_clump f Nil = Nil
|   map_clump f (Cons(tr,clp)) = (Cons((map_tree f tr),map_clump f clp ))
in map_tree f tr
end

(*max_list and height_clump are helper functions.max_list returns the max element of an integer list
height_clump returns a list of the height of the trees of the clump.
Therefore max_list(height_clump clmp) returns the max height of tree in the clump clmp
*)
fun height tr =
let 

fun max_list [] = 0
|   max_list (h::t)=
let val y=max_list t
in  if h>=y then h else y   end

fun height_tree Empty=0
|   height_tree (Node(node_val,cl))=1+max_list(height_clump cl [])
and 
height_clump Nil ll = ll
|   height_clump (Cons(t,clmp)) ll = height_clump clmp (height_tree(t)::ll)
in 
height_tree tr
end 

fun size tr =
let
fun sum_list []=0
|   sum_list (h::t)= h+sum_list(t)
fun size_tree Empty = 0
|   size_tree (Node(node_val,cl))=1+sum_list(size_clump cl [])
and 
size_clump Nil ll = ll
|   size_clump (Cons(t,clmp)) ll = size_clump clmp (size_tree(t)::ll)
in size_tree tr
end



(* val test4=preorder(Node(2,(Cons(Node(3,Nil),Nil))))
val test5=preorder(Node(1,Cons(Node(2,(Cons(Node(3,Nil),Nil))),Nil)))
val test6=Node(1,Cons(Node(2,(Cons(Node(3,Nil),Nil))),Nil))
val test8=Node(1,Cons(Node(2,(Cons(Node(3,Nil),Cons(Node(4,Nil),Cons(Node(5,Nil),Nil))))),Nil)) *)

end
(*
            1
            |
            2
          / | \ 
        3   4   5
*)