functor ApplicativeSyntax (A : APPLICATIVE) = struct
  local structure F = FunctorSyntax(A) in
    open F

    infix 4 <*>
    fun fs <*> gs = A.ap fs gs
  end
end
