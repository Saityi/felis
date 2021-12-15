signature COCARTESIAN = sig
  include PROFUNCTOR
  val left  : ('a, 'b) a -> (('a, 'c) Base.either, ('b, 'c) Base.either) a
  val right : ('a, 'b) a -> (('c, 'a) Base.either, ('c, 'b) Base.either) a
end
