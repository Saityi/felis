functor MonoidMethods (M : MONOID) = struct
  local
    structure S = MonoidSyntax(M)
    open Base
  in
    open M S
    fun mconcat xs = foldr (op @) (empty ()) xs
  end
end
