functor FunctorMethods (F : FUNCTOR) = struct
  local
    structure S = FunctorSyntax(F)
    open Base
    infix 6 <$>
  in
    open S

    infix 4 $>
    fun fa $> c = (const c) <$> fa

    infixr 4 <$
    fun c <$ fa = fa $> c

    infix 1 <&>
    fun fa <&> f = f <$> fa

    fun void fa = const () <$> fa
  end
end
