functor IntSumMonoid (structure I : INTEGER) : MONOID =
struct
  type 'a m = I.int

  fun empty () = I.fromInt 0
  fun append x y = I.+ (x, y)
end