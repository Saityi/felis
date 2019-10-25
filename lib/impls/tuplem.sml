functor TupleMonadInstance (structure Ma : MONOID; type b) : MONAD = 
struct
  type 'a m = (b Ma.m * 'a)
  
  fun map f (b, a) = (b, f a)

  fun pure x = (Ma.empty (), x)
  fun ap (u, f) (v, x) = (Ma.append u v, f x)
  
  fun bind f (u, a) = 
    let val (u', b) = f a in
    (Ma.append u' u, b)
    end
end