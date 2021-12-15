structure FunctionInstances = struct
  structure Arrow : ARROW = struct
    (* Is-a category *)
    type ('a, 'b) a = 'a -> 'b

    fun id x = x
    fun comp f g = f o g
    fun arr f = f
    fun first g (x, y) = (g x, y)
  end

  functor Contravariant (type b) : CONTRAVARIANT = struct
    type 'a m = 'a -> b
    fun contramap f g = g o f
  end

  functor Monad (type b) : MONAD = struct
    (* Is-a Functor, Is-an Applicative *)
    type 'a m = (b -> 'a)
    fun map f g = f o g
    fun pure v = fn _ => v
    fun ap f g r = f r (g r)
    fun bind k f = fn r => k (f r) r
  end

  structure Profunctor : PROFUNCTOR = struct
    type ('a, 'b) a = 'a -> 'b
    open Arrow
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

  structure Monoidal : MONOIDAL = struct
    open Profunctor
    fun par f g (a, c) = (f a, g c)
    fun empty () = Base.id
  end
end
