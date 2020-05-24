functor ApplicativeMethods (A : APPLICATIVE) = struct
  local
    structure S = ApplicativeSyntax(A)
    structure E = FunctorMethods(A)
    open Base
    infix 6 <$> <*>
  in
    open S E A

    infix 4 *>
    fun a1 *> a2 = (id <$ a1) <*> a2

    infixr 4 <*
    fun a2 <* a1 = a1 *> a2


    infix 4 <**>
    fun gs <**> fs = fs <*> gs

    fun liftA f a = A.pure f <*> a
    fun liftA2 f a b = (liftA f) a <*> b
    fun liftA3 f a b c = (liftA2 f) a b <*> c

    fun replicateA 0 fa = A.pure []
      | replicateA n fa = uncurry op :: <$> fa <*> replicateA (n - 1) fa
  end
end
