functor IntInstances (I : INTEGER) = struct
  local open I in
    structure Prod : MONOID = struct
      type 'a m = int

      fun empty () = fromInt 1
      fun append x y = x * y
    end

    structure Sum : GROUP = struct
      type 'a m = int

      fun empty () = fromInt 0
      fun append x y = x + y
      fun invert v = ~ v
    end
  end
end
