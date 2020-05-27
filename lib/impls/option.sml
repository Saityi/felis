functor OptionInstances (O : OPTION) =
struct
  structure Monad : MONAD = struct
    type 'a m = 'a O.option

    val map = O.map

    fun pure a = O.SOME a
    fun ap (O.SOME f) m = map f m
      | ap O.NONE m  = O.NONE

    fun bind f xs = O.mapPartial f xs
  end

  structure Foldable : FOLDABLE = struct
    type 'a m = 'a O.option

    fun foldr f z O.NONE = z
      | foldr f z (O.SOME v) = f v z
  end

  functor Traversable (A : APPLICATIVE) : TRAVERSABLE = struct
    structure A = A
    structure F = Foldable

    fun traverse f O.NONE = A.pure O.NONE
      | traverse f (O.SOME v) = A.map O.SOME (f v)
  end

end
