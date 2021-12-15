structure Tagged = struct
  type ('a, 'b) tagged = 'b

  fun tagged (b : 'b) : ('a, 'b) tagged = b
  fun unTagged (b : ('a, 'b) tagged) : 'b = b

  structure Profunctor : PROFUNCTOR = struct
    type ('a, 'b) a = ('a, 'b) tagged
    fun dimap f g b = tagged (g b)
  end

  structure Cocartesian : COCARTESIAN = struct
    open Profunctor
    type ('a, 'b) a = ('a, 'b) tagged
    fun left b = tagged (Base.left b)
    fun right b = tagged (Base.right b)
  end
  structure Monoidal : MONOIDAL = struct
    open Profunctor
    type ('a, 'b) a = ('a, 'b) tagged
    fun par b d = tagged (b, d)
    fun empty () = tagged ()
  end
end
