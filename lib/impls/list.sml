functor ListInstances (L : LIST) =
struct
  local open L Base in
    structure ListMonoid : MONOID =
    struct
      type 'a m = 'a list
      fun append xs ys = xs @ ys
      fun empty () = nil
    end

    structure ListMonad : MONAD =
    struct
      local open ListMonoid in
        type 'a m = 'a list
        val map = L.map

        fun pure a = a :: nil
        fun ap nil xs        = empty ()
          | ap (f :: fs) xs  = append (map f xs) (ap fs xs)
        
        fun bind f xs = concat (map f xs)
      end
    end

    structure ListAlternative : ALTERNATIVE =
    struct
      local open ListMonoid in
        open ListMonad
        type 'a m = 'a list
        val alt = append
        val empty = empty
      end
    end

    structure ListFoldable : FOLDABLE =
    struct
      type 'a m = 'a list

      fun foldr f = L.foldr (curry f)
    end

    structure ListTraversable : TRAVERSABLE =
    (* For lists of lists *)
    struct
      structure A = ListMonad
      structure F = ListFoldable
      local
        structure S = ApplicativeEnrichments(A)
        open S
        infix 6 <$> <*>
      in
        fun traverse f xs =
          let fun trav x ys = (uncurry op ::) <$> (f x) <*> ys
          in F.foldr trav (A.pure nil) xs
          end
      end
    end
  end
end