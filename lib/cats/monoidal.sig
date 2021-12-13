signature MONOIDAL = sig
  include PROFUNCTOR
  val par   : ('a, 'b) m -> ('c, 'd) m -> (('a * 'c), ('b * 'd)) m
  val empty : unit -> (unit, unit) m
end
