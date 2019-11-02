structure FunctionArrow : ARROW =
struct
  type ('a, 'b) a = 'a -> 'b

  fun id x = x
  fun comp f g = f o g
  fun arr f = f
  fun first g (x, y) = (g x, y)
end

(* TODO : Some sort of better encoding ... ? *)
functor FunctionMonad (type b) : MONAD =
struct
  type 'a m = (b -> 'a)
  val map = FunctionArrow.comp
  val pure = Base.const
  fun ap f g r = f r (g r)
  fun bind k f = fn r => k (f r) r
end