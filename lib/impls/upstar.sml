functor Upstar (F : FUNCTOR) = struct
  datatype ('a, 'b) upstar = upstar of ('a -> 'b F.m)

  structure Profunctor : PROFUNCTOR = struct
    type ('a, 'b) m = ('a, 'b) upstar

    fun dimap f g (upstar h) = upstar (F.map g o h o f)
  end

  structure Cartesian : CARTESIAN = struct
    type ('a, 'b) m = ('a, 'b) upstar
    open Profunctor

    fun first (upstar f) =
      upstar (fn (a, c) =>
                F.map (fn v => (v, c))
                      (f a))

    fun second (upstar f) =
      upstar (fn (c, a) =>
                F.map (fn v => (c, v))
                      (f a))
  end
end
