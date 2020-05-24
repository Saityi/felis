signature APPLICATIVE = sig
  include FUNCTOR

  val pure : 'a -> 'a m
  val ap  : ('a -> 'b) m -> 'a m -> 'b m
end
