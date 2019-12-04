functor TupleMInstances (type b
                         structure Ma : MONOID) = 
struct
  structure TupleMonad : MONAD = 
  struct
    type 'a m = (b Ma.m * 'a)
    
    fun map f (b, a) = (b, f a)

    fun pure x = (Ma.empty (), x)
    fun ap (u, f) (v, x) = (Ma.append u v, f x)
    
    fun bind f (u, a) = 
        let 
          val (u', b) = f a 
        in
          (Ma.append u' u, b)
        end
  end
  
  local structure M = TupleFInstances(type b = b) in
    structure TupleFoldable = M.TupleFoldable
  end

  structure TupleTraversable : TRAVERSABLE =
  struct
    structure A = TupleMonad
    structure F = TupleFoldable

    fun traverse f (x, y) = 
      A.map (fn y' => (x, y')) (f y)
  end
end