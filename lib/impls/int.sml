functor IntInstances (I : INTEGER) =
struct
  local open I in
    structure ProdMonoid : MONOID =
    struct
      type 'a m = int

      fun empty () = fromInt 1
      fun append x y = x * y
    end

    structure SumMonoid : MONOID =
    struct
        type 'a m = int

        fun empty () = fromInt 0
        fun append x y = x + y
    end
  end
end