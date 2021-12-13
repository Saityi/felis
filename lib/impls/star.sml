functor Star (structure A : APPLICATIVE) = struct
    type ('a, 'b) star = ('a -> 'b A.m)
    fun star (f : 'a -> 'b A.m) : ('a, 'b) star = f
    fun runStar (u : ('a, 'b) star) = u
    structure Profunctor : PROFUNCTOR = struct
      type ('a, 'b) m = ('a, 'b) star
      fun dimap f g (h : ('a, 'b) star) : ('s, 't) star = star (A.map g o h o f)
    end

    structure Cartesian : CARTESIAN = struct
      type ('a, 'b) m = ('a, 'b) star
      open Profunctor

      fun first (f : ('a, 'b) star) =
        star (fn (a, c) =>
                  A.map (fn v => (v, c))
                        (f a))

      fun second (f : ('a, 'b) star) =
        star (fn (c, a) =>
                  A.map (fn v => (c, v))
                        (f a))
    end
    structure Cocartesian : COCARTESIAN = struct
      type ('a, 'b) m = ('a, 'b) star
      open Profunctor
      fun right (f : ('a, 'b) star) =
        star (Base.either (A.map Base.left o A.pure) (A.map Base.right o f))
      fun left (f : ('a, 'b) star) =
        star (Base.either (A.map Base.left o f) (A.map Base.right o A.pure))
    end

    structure Monoidal : MONOIDAL = struct
      open Profunctor
      local structure AM = ApplicativeMethods(A)
            structure AS = ApplicativeSyntax(A)
            open AM AS Base
            infix 4 <$>
            infix 4 <*>
      in
        type ('a, 'b) m = ('a, 'b) star

        fun empty () = star A.pure

        fun par (f : ('a, 'b) star) (g : ('s, 't) star) =
          star (fn (a, b) => pair <$> f a <*> g b)
      end
    end
end
