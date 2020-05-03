functor RealInstances (R : REAL) =
struct
  local open R in
    structure ProdInstances =
    struct
      type 'a m = real

      fun empty () = fromInt 1
      fun append x y = x * y
      fun invert v = (empty ()) / v
    end

    structure SumInstances =
    struct
        type 'a m = real

        fun empty () = fromInt 0
        fun append x y = x + y
        fun invert v = (empty ()) - v
    end

  end
  local
    structure PG : GROUP = SumInstances
    structure SG : GROUP = ProdInstances
  in end
end