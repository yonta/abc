(* functor ModularInt (val maxInt : int val modulo : int) : MODULAR_INT = *)
signature MODULAR_INT =
sig
  val modulo : int
  val toModular : int -> int
  val pow : int -> int -> int
  val nCk : int -> int -> int
  val inverse : int -> int
  val factorial : int -> int
end

structure ModularInt : MODULAR_INT =
struct

  local
    structure Array = Array31
  in

  val maxInt = 1000000000       (* 10^9 * 2*)
  val modulo = 1000000007       (* 10^9 + 7 *)

  (* open IntInf *)
  type int = int

  fun toModular n = n mod modulo

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
  (* val maxInt = 0 *)
  (* val modulo = IntInf.pow (10, 9) + 7 *)
  (* doModulo : intInf -> int *)
  fun doModulo n = IntInf.toInt (IntInf.mod (n, IntInf.fromInt modulo))
  fun pow e x = doModulo (IntInf.pow (IntInf.fromInt e, x))
  fun inverseImpl n =
      case Array.sub (invTable, n) of
          NONE =>
          let
            val infA = IntInf.fromInt (inverseImpl (modulo mod n))
            val infB = IntInf.fromInt (modulo div n)
            val intC = doModulo (infA * infB)
            val invN = modulo - intC
            val () = Array.update (invTable, n, SOME invN)
          in
            invN
          end
        | SOME x => x
  fun inverse n = if n < 0 orelse maxInt < n then raise Size
                  else inverseImpl n
  (* factorial n = (factorial (n - 1) * n) mod modulo *)
  fun factorialImpl n =
      case Array.sub (facTable, n) of
          NONE =>
          let
            val infA = IntInf.fromInt (factorialImpl (n - 1)) * IntInf.fromInt n
            val facN = doModulo infA
            val () = Array.update (facTable, n, SOME facN)
          in
            facN
          end
        | SOME x => x
  fun factorial n = if n < 0 orelse maxInt < n then raise Size
                    else factorialImpl n
  fun nCk n k =
      factorial n * inverse (factorial k) * inverse (factorial (n - k))

  end (* local *)
end (* struct *)
