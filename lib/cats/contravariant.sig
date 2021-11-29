signature Contravariant = sig
  type 'a m
  val contramap : ('a -> 'b) -> 'b m -> 'a m
end
