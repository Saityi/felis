signature MONAD = sig
  include APPLICATIVE

  val bind : ('a -> 'b m) -> 'a m -> 'b m
end
