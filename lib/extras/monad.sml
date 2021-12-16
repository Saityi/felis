functor MonadMethods (M : MONAD) = struct
  local
    structure S = MonadSyntax(M)
    structure A = ApplicativeMethods(M)
    infix 1 >>=
  in
    open S A
    fun join ma = ma >>= Base.id
    fun liftM f m = m >>= pure o f
    fun return v = M.pure v
    fun >> ((fa : 'a M.m), (fb : 'b M.m)) : 'b M.m = fa >>= (fn _ => fb)
  end
end
