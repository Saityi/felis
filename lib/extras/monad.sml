functor MonadMethods (M : MONAD) = struct
  local
    structure S = MonadSyntax(M)
    structure A = ApplicativeMethods(M)
    infix 1 >>=
  in
    open S A
    fun join ma = ma >>= Base.id
    fun liftM f m = m >>= pure o f
  end
end
