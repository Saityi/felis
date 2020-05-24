functor FoldableMethods (F : FOLDABLE) = struct
  local open Base F in
    infix 0 $

    fun toList vs =
      F.foldr (uncurry op ::) [] vs

    fun null vs =
      let
        fun null' anything _ = false
      in
        F.foldr null' true vs
      end
    fun elem v vs =
      let
        fun isElem v' found = found orelse (v = v')
      in
        F.foldr isElem false vs
      end

    fun foldl f vi vs =
      let
        fun fold' b g x = g (f x b)
      in
        F.foldr fold' id vs vi
      end
  end
end

functor FoldableMonoidalMethods (structure F : FOLDABLE
                                 structure M : MONOID) = struct
  local
    structure FE = FoldableMethods(F)
    structure ME = MonoidMethods(M)
  in
    open FE ME
    fun foldMap f vs =
      let
        fun foldm' v acc = M.append (f v) acc
      in
        F.foldr foldm' (M.empty ()) vs
      end

    fun fold vs = foldMap Base.id vs
  end
end
