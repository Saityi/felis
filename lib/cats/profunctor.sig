signature PROFUNCTOR = sig
  type ('a, 'b) m
  val dimap : ('a -> 'b) -> ('c -> 'd) -> ('b, 'c) m -> ('a, 'd) m
end
