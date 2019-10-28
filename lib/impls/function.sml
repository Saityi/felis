structure FunctionInstances = 
struct
  structure FunctionArrow : ARROW =
  struct
    type ('a, 'b) a = 'a -> 'b

    fun id x = x
    fun comp f g = f o g
    fun arr f = f
    fun first g (x, y) = (g x, y)
  end

  (* TODO: ?
   * structure FunctionMonad : MONAD =
   * struct
   *  type 'a m = ?
   *  val map = FunctionArrow.comp
   *  val pure = Base.const
   *  fun ap f g = fn x => f x (g x)
   *  fun bind f k = fn r => k (f r) r
   * end *)
    
end