functor MonadEnrichments (M : MONAD) = 
struct
  local
    structure S = MonadSyntax(M)
    structure A = ApplicativeEnrichments(M)
    infix 1 >>=
  in
    open S A
    fun join ma = ma >>= Base.id
  end
end