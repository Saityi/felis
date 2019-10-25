functor CategoryEnrichments (C : CATEGORY) =
struct
  infixr 6 >>> <<<
  
  fun f >>> g = C.comp g f
  fun f <<< g = C.comp f g
end