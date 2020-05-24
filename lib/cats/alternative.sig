signature ALTERNATIVE = sig
  include APPLICATIVE

  val empty : unit -> 'a m
  val alt : 'a m -> 'a m -> 'a m
end
