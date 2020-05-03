functor IntInstances (I : INTEGER) =
struct
  local open I in
    structure ProdInstances =
    struct
      type 'a m = int

      fun empty () = fromInt 1
      fun append x y = x * y
    end

    structure SumInstances =
    struct
        type 'a m = int

        fun empty () = fromInt 0
        fun append x y = x + y
        fun invert v = ~ v
    end

  end
  local
    structure PM : MONOID = ProdInstances
    structure PG : GROUP = SumInstances
  in end
end