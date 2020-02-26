(*
functor MemoizedModularInt (val maxInt : int val modulo : int) : MODULAR_INT =
 *)
structure MemoizedModularInt : MEMOIZED_MODULAR_INT =
struct

  val maxInt = 100000           (* 10^5 *)
  val modulo = 1000000007       (* 10^9 + 7 *)
  val moduloInf = IntInf.fromInt modulo

  type int = Int.int

  fun toModular n = n mod modulo

  (* doModulo : intInf -> int *)
  fun doModulo n = IntInf.toInt (IntInf.mod (n, IntInf.fromInt modulo))

  fun op + (x, y) = doModulo (IntInf.+ (IntInf.fromInt x, IntInf.fromInt y))

  fun op - (x, y) =
      doModulo
        (IntInf.+ (IntInf.- (IntInf.fromInt x, IntInf.fromInt y), moduloInf))

  fun op * (x, y) = doModulo (IntInf.* (IntInf.fromInt x, IntInf.fromInt y))

  fun op mod (x, y) = Int.mod (x, y)

  fun powImpl acc e 0 = acc
    | powImpl acc e x =
      if x mod 2 = 0
      then
        let
          val newE = e * e
          val newX = Int.div (x, 2)
        in
          powImpl acc newE newX
        end
      else
        let
          val newAcc = acc * e
          val newX = Int.- (x, 1)
        in
          powImpl newAcc e newX
        end
  fun pow e x = powImpl 1 e x

  (* n! table *)
  val facTable =
      let
        val arr = Array.array (maxInt, NONE)
        val () = Array.update (arr, 0, SOME 1)
        val () = Array.update (arr, 1, SOME 1)
      in
        arr
      end
  (* n^(-1) table *)
  val invTable =
      let
        val arr = Array.array (maxInt, NONE)
        val () = Array.update (arr, 0, SOME 1)
        val () = Array.update (arr, 1, SOME 1)
      in
        arr
      end
  (* (n!)^(-1) table, 0 access is error *)
  (*
  val factInvTable =
      let
        val arr = Array.array (maxInt, NONE)
        val () = Array.update (arr, 1, SOME 1)
      in
        arr
      end
  *)

  fun inverseImpl n =
      case Array.sub (invTable, n) of
          NONE =>
          let
            val invN = modulo - inverseImpl (modulo mod n) * (modulo div n)
            val () = Array.update (invTable, n, SOME invN)
          in
            invN
          end
        | SOME x => x
  fun inverse n = if n < 0 orelse maxInt < n then raise Domain
                  else inverseImpl n

  fun op div (x, y) = x * inverse y

  fun factorialImpl n =
      case Array.sub (facTable, n) of
          NONE =>
          let
            val facN = n * factorialImpl (Int.- (n, 1))
            val () = Array.update (facTable, n, SOME facN)
          in
            facN
          end
        | SOME x => x
  fun factorial n = if n < 0 orelse maxInt < n then raise Size
                    else factorialImpl n

  fun nPkImpl acc n 0 = acc
    | nPkImpl acc n k = nPkImpl (acc * n) (Int.- (n, 1)) (Int.- (k, 1))
  fun nPk n k = nPkImpl 1 n k

  fun nCk n k =
      factorial n * inverse (factorial k) * inverse (factorial (n - k))

end (* struct *)
