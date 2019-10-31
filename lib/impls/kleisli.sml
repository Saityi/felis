functor Kleisli (structure M : MONAD) : ARROW =
struct
  datatype ('a, 'b) a = kleisli of ('a -> ('b M.m))

  val id = kleisli (fn x => M.pure x)
  fun comp (kleisli f) (kleisli g) =
    kleisli (fn b => M.bind f (g b))
  
  fun arr f = kleisli (M.pure o f)
  fun first (kleisli f) = 
    let fun k (b, d) =
      M.bind (fn c => M.pure (c, d)) (f b)
    in kleisli k end
end