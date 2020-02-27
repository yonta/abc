structure ModularInt : MODULAR_INT =
struct
  val modulo = 1000000007       (* 10^9 + 7 *)
  val moduloInf = IntInf.fromInt modulo

  type int = int

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

  fun inverse n = pow n (Int.- (modulo, 2))

  fun op div (x, y) = x * inverse y

  fun nPkImpl acc n 0 = acc
    | nPkImpl acc n k = nPkImpl (acc * n) (Int.- (n, 1)) (Int.- (k, 1))
  fun nPk n k = if n < k then raise Domain else nPkImpl 1 n k

  fun nCk n k =
      if n < k then raise Domain
      else if n < k * 2 then nCk n (n - k)
      else (nPk n k) div (nPk k k)

  fun factorialImpl acc 0 = acc
    | factorialImpl acc n = factorialImpl (acc * n) (Int.- (n, 1))
  fun factorial n =
      if n > 500000 then raise Fail "do not call factorial with bigger arg"
      else factorialImpl 1 n

end (* struct *)
