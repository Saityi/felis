signature PROFUNCTOR = sig
  type ('a, 'b) a

  val dimap : ('a -> 'b) -> ('c -> 'd) -> ('b, 'c) a -> ('a, 'd) a
end
