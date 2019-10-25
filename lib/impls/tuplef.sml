functor TupleFunctorInstance (type b) : FUNCTOR = 
struct
  type 'a m = (b * 'a)

  fun map f (b, a) = (b, f a)
end