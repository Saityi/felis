signature CARTESIAN = sig
  (* AKA Strong Profunctor *)
  include PROFUNCTOR
  val first  : ('a, 'b) m -> (('a * 'c), ('b * 'c)) m
  val second : ('a, 'b) m -> (('c * 'a), ('c * 'b)) m
end
