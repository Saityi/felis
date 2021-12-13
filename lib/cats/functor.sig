signature FUNCTOR = sig
  include HKT

  val map : ('a -> 'b) -> 'a m -> 'b m
end
