functor ListInstances (L : LIST) = struct
  local open L Base in

    structure Monoid : MONOID = struct
      type 'a m = 'a list
      fun append xs ys = xs @ ys
      fun empty () = nil
    end

    structure Monad : MONAD = struct
      local open Monoid in
        type 'a m = 'a list
        val map = L.map

        fun pure a = a :: nil
        fun ap nil xs        = empty ()
          | ap (f :: fs) xs  = append (map f xs) (ap fs xs)

        fun bind f xs = concat (map f xs)
      end
    end

    structure Alternative : ALTERNATIVE = struct
      local open Monoid in
        open Monad
        type 'a m = 'a list
        val alt = append
        val empty = empty
      end
    end

    structure Foldable : FOLDABLE = struct
      type 'a m = 'a list

      fun foldr f = L.foldr (curry f)
    end

    functor Traversable (A : APPLICATIVE) : TRAVERSABLE = struct
      (* For lists of effects *)

      structure A = A
      structure F = Foldable
      fun traverse f xs =
        let
          fun trav x ys = A.ap (A.map (uncurry op ::) (f x)) ys
        in
          F.foldr trav (A.pure nil) xs
        end
    end
  end
end
