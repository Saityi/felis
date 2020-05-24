functor RealInstances (R : REAL) = struct
  local open R in
    structure Prod : GROUP = struct
      type 'a m = real

      fun empty () = fromInt 1
      fun append x y = x * y
      fun invert v = (empty ()) / v
    end

    structure Sum : GROUP = struct
      type 'a m = real

      fun empty () = fromInt 0
      fun append x y = x + y
      fun invert v = (empty ()) - v
    end

  end
end
