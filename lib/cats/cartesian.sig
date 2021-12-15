signature CARTESIAN = sig
  (* AKA Strong Profunctor *)
  include PROFUNCTOR
  val first  : ('a, 'b) a -> (('a * 'c), ('b * 'c)) a
  val second : ('a, 'b) a -> (('c * 'a), ('c * 'b)) a
end
