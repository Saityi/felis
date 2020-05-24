functor CategoryMethods (C : CATEGORY) = struct
  infixr 1 >>> <<<

  fun f >>> g = C.comp g f
  fun f <<< g = C.comp f g
end
