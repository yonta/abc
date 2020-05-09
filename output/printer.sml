structure Printer
          :sig
            type 'elem fmt
            type 'elem p = 'elem -> 'elem fmt
            val p : 'elem fmt -> unit
            val pi : int p
            val pc : char p
            val pr : real p
            val pb : bool p
            val po : 'elem p -> 'elem option p
            val p2 : 'elem1 p * 'elem2 p -> 'elem1 * 'elem2 -> 'elem fmt
            val p3 : 'elem1 p * 'elem2 p * 'elem3 p ->
                     'elem1 * 'elem2 * 'elem3 -> 'elem fmt
            val p4 : 'elem1 p * 'elem2 p * 'elem3 p * 'elem4 p ->
                     'elem1 * 'elem2 * 'elem3 * 'elem4 -> 'elem fmt
            val pl : 'elem p -> 'elem list p
            val parr : 'elem p -> 'elem array p
            val pvec : 'elem p -> 'elem vector p
          end
=
struct
  (* p is a fomat to print *)
  type 'elem fmt = string list
  type 'elem p = 'elem -> 'elem fmt
  fun p strs = (app print strs; print "\n")
  fun pi n = [Int.toString n]
  fun pc c = [Char.toString c]
  fun pr r = [Real.toString r]
  fun pb b = [Bool.toString b]
  fun po pe NONE = ["NONE"]
    | po pe (SOME x) = "SOME " :: pe x

  local
    fun withHeadComma pelem (elem, accum) = "," :: pelem elem @ accum
    fun removeComma ("," :: l) = l | removeComma l = l
    fun makeContainerPrinter opener closer foldr pelem elems =
        opener :: removeComma (foldr (withHeadComma pelem) [closer] elems)
    fun id x = x
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
