functor TupleAInstances (type b) = 
struct
  structure TupleFunctor : FUNCTOR = struct
    type 'a m = (b * 'a)

    fun map f (b, a) = (b, f a)
  end

  structure TupleFoldable : FOLDABLE = struct
    type 'a m = (b * 'a)

    fun foldr f z (b, a) = f a z
  end
end