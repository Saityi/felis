functor MonadSyntax (M : MONAD) = 
struct
  local structure A = ApplicativeSyntax(M) in
    open A

    infix 1 >>=
    fun f >>= ma = M.bind f ma  
  end
end
