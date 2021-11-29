structure FunctionInstances = struct
  structure Arrow : ARROW = struct
    type ('a, 'b) a = 'a -> 'b

    fun id x = x
    fun comp f g = f o g
    fun arr f = f
    fun first g (x, y) = (g x, y)
  end

  functor Monad (type b) : MONAD = struct
    type 'a m = (b -> 'a)
    val map = Arrow.comp
    val pure = Base.const
    fun ap f g r = f r (g r)
    fun bind k f = fn r => k (f r) r
  end

  structure Profunctor : PROFUNCTOR = struct
    type ('a, 'b) m = 'a -> 'b
    fun dimap f g h = g o h o f
  end

  structure Cartesian : CARTESIAN = struct
    open Profunctor
    fun first f (a, c) = (f a, c)
    fun second f (c, a) = (c, f a)
  end

  structure Cocartesian : COCARTESIAN = struct
    open Profunctor
    local structure B = Base in
    fun left f = B.either (B.left o f) B.right
    fun right f = B.either B.left (B.right o f)
    end
  end
end
