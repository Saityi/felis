functor ApplicativeEnrichments (A : APPLICATIVE) = 
struct
  local
    structure S = ApplicativeSyntax(A)
    structure E = FunctorEnrichments(A)
    open Base
    infix 6 <$> <*>
  in
    open S E

    infix 4 *>
    fun a1 *> a2 = (id <$ a1) <*> a2

    infixr 4 <*
    fun a2 <* a1 = a1 *> a2


    infix 4 <**>
    fun gs <**> fs = fs <*> gs

    fun liftA f a = A.pure f <*> a
    fun liftA2 f a b = f a <*> b
    fun liftA3 f a b c = liftA2 f a b <*> c
  end
end