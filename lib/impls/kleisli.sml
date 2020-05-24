functor Kleisli (structure M : MONAD) : ARROW = struct
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
