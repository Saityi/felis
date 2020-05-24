functor AlternativeSyntax (A : ALTERNATIVE) = struct
  local structure AS = ApplicativeSyntax(A) in
    open AS

    infix 4 <|>
    fun qs <|> ps = A.alt qs ps
  end
end
