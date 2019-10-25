functor VectorInstances(structure V : VECTOR) =
struct
  structure VectorMonoid : MONOID =
  struct
    type 'a m = 'a V.vector
    
    fun append vs ws = V.concat [vs, ws]
    fun empty () = V.fromList []
  end

  structure VectorMonad : MONAD =
  struct
    type 'a m = 'a V.vector

    val map = V.map

    fun pure a = V.fromList [a]
    fun ap fs vs =
      let fun ap' (f, acc) = VectorMonoid.append (V.map f vs) acc
          val start = V.fromList []
      in V.foldr ap' start fs
      end

    fun bind f vs =
      V.concat (V.foldr (fn (v, acc) => f v :: acc) [] vs)
  end
end