signature MODULAR_INT =
sig
  val modulo : int
  val toModular : int -> int
  val pow : int -> int -> int
  val nCk : int -> int -> int
  val inverse : int -> int
  val factorial : int -> int
end
