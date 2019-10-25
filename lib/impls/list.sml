functor ListInstances (structure L : LIST) =
struct
  local open L in
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
  end
end