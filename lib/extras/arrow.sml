functor ArrowMethods (A : ARROW) = struct
  local
    structure CE = CategoryMethods(A)
    open Base
  in
    open CE
    infixr 3 *** &&&
    infixr 1 <<< >>> ^>> >>^ ^<< <<^

    fun second f =
      A.arr swap >>>
      A.first f >>>
      A.arr swap

    fun f *** g =
      A.first f >>>
      A.arr swap >>>
      A.first g >>>
      A.arr swap

    fun f &&& g =
      A.arr dupe >>> f *** g

    fun returnA () = A.arr id

    fun f ^>> a = A.arr f >>> a
    fun f ^<< a = A.arr f <<< a

    fun a >>^ f = a >>> A.arr f
    fun a <<^ f = a <<< A.arr f
  end
end
