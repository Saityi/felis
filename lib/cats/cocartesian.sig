signature COCARTESIAN = sig
  include PROFUNCTOR
  val left  : ('a, 'b) m -> (('a, 'c) Base.either, ('b, 'c) Base.either) m
  val right : ('a, 'b) m -> (('c, 'a) Base.either, ('c, 'b) Base.either) m
end
