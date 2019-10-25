functor AlternativeEnrichments (A : ALTERNATIVE) =
struct
  local
    structure S = AlternativeSyntax(A)
    structure E = ApplicativeEnrichments(A)
    open Base
    infix 6 <$> <*> <|>
  in
    open S E

    fun some v = uncurry op :: <$> v <*> many v
    and many v = some v <|> A.pure []
  end
end