functor FunctorSyntax (F : FUNCTOR) = struct
  infix 4 <$>
  fun f <$> fa = F.map f fa
end
