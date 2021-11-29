signature COMONAD = sig
  include FUNCTOR

  val extract : 'a m -> 'a
  val extend  : ('a m -> 'b) -> ('a m -> 'b m)
end
