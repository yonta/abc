signature MINIMUM_ARRAY =
sig
  eqtype 'a array
  val maxLen : int
  val array : int * 'a -> 'a array
  val sub: 'a array * int -> 'a
  val update: 'a array * int * 'a -> unit
end
