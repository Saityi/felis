signature FOLDABLE = sig
  type 'a m

  val foldr : ('a -> 'b -> 'b) -> 'b -> 'a m -> 'b
end
