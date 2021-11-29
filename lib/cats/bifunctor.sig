signature BIFUNCTOR = sig
  type ('a, 'b) m
  val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) m -> ('b, 'd) m
end
