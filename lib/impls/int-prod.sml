functor IntProdMonoid (structure I : INTEGER) : MONOID =
struct
  type 'a m = I.int

  fun empty () = I.fromInt 1
  fun append x y = I.* (x, y)
end