signature MODULAR_INT =
sig
  type int = int
  val modulo : int
  val toModular : Int.int -> int
  val + : int * int -> int
  val - : int * int -> int
  val * : int * int -> int
  val div : int * int -> int
  val mod : int * int -> int
  val pow : int -> int -> int
  val nPk : int -> int -> int
  val nCk : int -> int -> int
  val inverse : int -> int
  val factorial : int -> int
end
