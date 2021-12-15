signature MONOIDAL = sig
  include PROFUNCTOR
  val par   : ('a, 'b) a -> ('c, 'd) a -> (('a * 'c), ('b * 'd)) a
  val empty : unit -> (unit, unit) a
end
