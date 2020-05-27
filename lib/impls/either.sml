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

    structure Foldable : FOLDABLE = struct
      type 'a m = (b, 'a) either
      fun foldr f z (left e) = z
        | foldr f z (right e) = f e z
    end

    functor Traversable (A : APPLICATIVE) : TRAVERSABLE = struct
      structure A = A
      structure F = Foldable

      fun traverse f (left e) = A.pure (left e)
        | traverse f (right e) = A.map right (f e)
    end
  end
end
