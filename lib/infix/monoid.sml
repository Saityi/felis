functor MonoidSyntax (M : MONOID) = struct
  fun xs @ ys = M.append xs ys
end
