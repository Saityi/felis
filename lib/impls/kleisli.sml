functor Kleisli (structure M : MONAD) : ARROW =
struct
  local 
    structure MSyntax = MonadSyntax(M)
    open MSyntax
    infix 1 >>=
  in
    datatype ('a, 'b) a = kleisli of { run_kleisli : ('a -> ('b M.m)) }

    val id = kleisli { run_kleisli = M.pure }
    
    fun comp (kleisli { run_kleisli = f }) 
             (kleisli { run_kleisli = g }) =
      let fun k b = (g b) >>= f
      in kleisli { run_kleisli = k }
      end
    
    fun arr f = 
      kleisli { run_kleisli = M.pure o f }
    
    fun first (kleisli { run_kleisli = f }) = 
      let fun k (b, d) = (f b) >>= (fn c => M.pure (c, d))
      in kleisli { run_kleisli = k }
      end
  end
end