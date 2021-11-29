functor ComonadMethods (W : COMONAD) = struct
  local open Base in
  fun duplicate f = W.extend id f
  end
end
