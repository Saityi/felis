functor KleisliArrow (structure M : MONAD) : ARROW = struct
  local
    structure MSyntax = MonadSyntax(M)
    open MSyntax
    infix 1 >>=
  in
    type ('a, 'b) a = ('a -> ('b M.m))

    val id = M.pure
    fun comp f g b =
      g b >>= f

    fun arr f = M.pure o f
    fun first f (b, d) =
      f b >>= (fn c => M.pure (c, d))
  end
end

functor KleisliProfunctor (structure M : MONAD) : PROFUNCTOR = struct
  local
    structure MSyntax = MonadSyntax(M)
    structure MonadFuncs = MonadMethods(M)
    open MonadFuncs
    open MSyntax
    infix 1 >>=
    open Base
    infix 9 $
  in
  type ('a, 'b) m = ('a -> ('b M.m))
  fun dimap f g h = liftM $ g o h o f
  end
end
