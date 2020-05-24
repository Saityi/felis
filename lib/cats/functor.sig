signature FUNCTOR = sig
  type 'a m

  val map : ('a -> 'b) -> 'a m -> 'b m
end
