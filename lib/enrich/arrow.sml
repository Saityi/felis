functor ArrowEnrichments (A : ARROW) = 
struct
  local
    structure CE = CategoryEnrichments(A)
    open Base
  in
    open CE
    infix 6 >>> *** &&&

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
      A.arr (fn b => (b, b)) >>> 
      f *** g
  end
end