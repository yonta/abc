structure Printer
          :sig
            type fmt
            type 'elem p = 'elem -> fmt
            val p : fmt -> unit
            val pi : int p
            val pc : char p
            val pr : real p
            val pb : bool p
            val po : 'elem p -> 'elem option p
            val p2 : 'elem1 p * 'elem2 p -> 'elem1 * 'elem2 -> fmt
            val p3 : 'elem1 p * 'elem2 p * 'elem3 p ->
                     'elem1 * 'elem2 * 'elem3 -> fmt
            val p4 : 'elem1 p * 'elem2 p * 'elem3 p * 'elem4 p ->
                     'elem1 * 'elem2 * 'elem3 * 'elem4 -> fmt
            val pl : 'elem p -> 'elem list p
            val parr : 'elem p -> 'elem array p
            val pvec : 'elem p -> 'elem vector p
          end
=
struct
  datatype ptree = Leaf of string
                 | Node of ptree * ptree list * ptree
  type fmt = ptree
  type 'elem p = 'elem -> fmt   (* type p is fomated string to print *)
  local
    fun pImpl (Leaf s) = print s
      | pImpl (Node (p1, ps, p2)) =
        (pImpl p1; app pImpl ps; pImpl p2)
  in
  fun p fmt = (pImpl fmt; print "\n")
  end

  fun pi n = Leaf (Int.toString n)
  fun pc c = Leaf (Char.toString c)
  fun pr r = Leaf (Real.toString r)
  fun pb b = Leaf (Bool.toString b)
  fun po pe NONE = Leaf "NONE"
    | po pe (SOME x) = Node (Leaf "SOME ", nil, pe x)

  local
    fun plImpl pe nil = nil
      | plImpl pe [x] = [pe x]
      | plImpl pe (x :: xs) = pe x :: Leaf "," :: plImpl pe xs
  in
  fun pl pe l = Node (Leaf "[", plImpl pe l, Leaf "]")
  end

  local
    fun id x = x
    fun withHeadComma pe (elem, accum) = Leaf "," :: pe elem :: accum
    fun removeComma (Leaf "," :: l) = l | removeComma l = l
    fun makeContainerPrinter opener closer foldr pe container =
        let
          val ptrees = foldr (withHeadComma pe) nil container
          val ptrees = removeComma ptrees
        in
          Node (Leaf opener, ptrees, Leaf closer)
        end
  in
  fun p2 (pe1, pe2) (x1, x2) =
      makeContainerPrinter "(" ")" foldr id [pe1 x1, pe2 x2]
  fun p3 (pe1, pe2, pe3) (x1, x2, x3) =
      makeContainerPrinter "(" ")" foldr id [pe1 x1, pe2 x2, pe3 x3]
  fun p4 (pe1, pe2, pe3, pe4) (x1, x2, x3, x4) =
      makeContainerPrinter "(" ")" foldr id [pe1 x1, pe2 x2, pe3 x3, pe4 x4]
  fun pl pe l = makeContainerPrinter "[" "]" foldr pe l
  fun parr pe arr = makeContainerPrinter "<" ">" Array.foldr pe arr
  fun pvec pe vec = makeContainerPrinter "<|" "|>" Vector.foldr pe vec
  end
end

(* val % : ('a -> 'b) * 'a -> 'b *)
infixr 0 %
fun op % (f, g) = f g

(* example
local
  open Printer
in
val () = p% parr (po pi) (Array.fromList [NONE, SOME 1, SOME 2])
end
 *)
