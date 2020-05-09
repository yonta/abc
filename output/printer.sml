structure Printer
          : sig
            type 'elem printer = 'elem -> unit
            type 'elem generator = ('elem -> string) -> 'elem printer
            val generator : 'elem generator
            val p : 'elem printer
            val pi : int printer
            val pc : char printer
            val pr : real printer
            val pb : bool printer
            val po : 'elem printer -> 'elem option printer
            val pList : 'elem printer -> 'elem list printer
            val pArr : 'elem printer -> 'elem array printer
            val pVec : 'elem printer -> 'elem vector printer
          end
=
struct
  type 'elem printer = 'elem -> unit
  type 'elem generator = ('elem -> string) -> 'elem printer
  fun p makeContainerPrinter = (makeContainerPrinter; print "\n")

  fun generator toString elem = (print o toString) elem
  fun pi n = generator Int.toString n
  fun pc c = generator Char.toString c
  fun pr r = generator Real.toString r
  fun pb b = generator Bool.toString b
  fun po pElem NONE = print "NONE"
    | po pElem (SOME x) = (print "SOME "; pElem x)

  fun pListElem pElem nil = ()
    | pListElem pElem [e] = pElem e
    | pListElem pElem (h::t) = (pElem h; print ","; pListElem pElem t)
  fun pList pElem l = (print "["; pListElem pElem l; print "]")

  fun pArrAndVec opener separater closer appi elemPrinter elems =
      (print opener;
       appi
         (fn (0, elem) => elemPrinter elem
           | (_, elem) => (print separater; elemPrinter elem))
         elems;
       print closer)
  fun pArr pElem arr = pArrAndVec "<" "," ">" Array.appi pElem arr
  fun pVec pElem vec = pArrAndVec "<|" "," "|>" Vector.appi pElem vec
end

infixr 0 \
infix 1 %
fun op \ (makeContainerPrinter, g) = makeContainerPrinter g
fun op % (makeContainerPrinter, g) = makeContainerPrinter g
 
(* example
local
  open Printer
in
val () = p% pArr (po pi) (Array.fromList [NONE, SOME 1, SOME 2])
end
 *)
