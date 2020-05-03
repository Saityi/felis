functor VectorInstances(V : VECTOR) =
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
      let 
        fun ap' (f, acc) = VectorMonoid.append (V.map f vs) acc
        val start = V.fromList []
      in 
        V.foldr ap' start fs
      end

    fun bind f vs =
      V.concat (V.foldr (fn (v, acc) => f v :: acc) [] vs)
  end

  structure VectorFoldable : FOLDABLE =
  struct
    type 'a m = 'a V.vector

    fun foldr f = V.foldr (Base.curry f)
  end

  structure VectorTraversable : TRAVERSABLE =
    struct
      structure A = VectorMonad
      structure F = VectorFoldable
      local
        structure S = ApplicativeMethods(A)
        open S Base
        infix 6 <$> <*>
      in
        fun traverse f xs =
          let 
            fun trav x ys = (uncurry op ::) <$> (f x) <*> ys
          in 
            A.map V.fromList (F.foldr trav (A.pure []) xs)
          end
      end
    end
end