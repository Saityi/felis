functor TupleFInstances (type b) = struct
  structure Functor : FUNCTOR = struct
    type 'a m = (b * 'a)

    fun map f (b, a) = (b, f a)
  end

  structure Foldable : FOLDABLE = struct
    type 'a m = (b * 'a)

    fun foldr f z (b, a) = f a z
  end
end
