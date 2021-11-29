functor ProfunctorMethods (P : PROFUNCTOR) = struct
  local open Base in
  fun lmap f = P.dimap f id
  fun rmap f = P.dimap id f
  end
end
