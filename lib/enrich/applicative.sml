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

    fun liftA2 f x y = f <$> x <*> y
  end
end