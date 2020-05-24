functor EitherInstances (type b) = struct
  local open Base in
    structure Monad : MONAD = struct
      type 'a m = (b, 'a) either

      fun map f (right v) = right (f v)
        | map f (left e)  = left e

      val pure = right
      fun ap (right f) v  = map f v
        | ap (left v) _ = left v

      fun bind f (right ra) = f ra
        | bind f (left v) = left v
    end
  end
end
