signature CARTESIAN = sig
  include PROFUNCTOR
  val first  : ('a, 'b) m -> (('a * 'c), ('b * 'c)) m
  val second : ('a, 'b) m -> (('c * 'a), ('c * 'b)) m
end
