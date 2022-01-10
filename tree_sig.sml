signature tree =
sig
    type 'a tree
    val preorder : 'a tree -> 'a list
    val postorder : 'a tree -> 'a list
    val map : ('a -> 'b) -> 'a tree -> 'b tree
    val height : 'a tree -> int
    val size : 'a tree -> int
end
