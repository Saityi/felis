functor MonadEnrichments (M : MONAD) = 
struct
  local
    structure S = MonadSyntax(M)
  in
    open S
    fun join ma = ma >>= (fn m => m)
  end
end