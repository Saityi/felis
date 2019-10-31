(* This provides a set of common functions and
 * higher order functions *)
structure Base = 
struct
  datatype ('a, 'b) either = left of 'a
                           | right of 'b
  fun id x = x
  fun const a b = a
  fun uncurry f a b = f (a, b)
  fun curry f (a, b) = f a b
  fun flip f x y = f y x
  fun flipu f (x, y) = f (y, x)
  fun swap (x, y) = (y, x)

  infix 9 $
  fun f $ x = f x

  infix 6 |>
  fun x |> f = f x
end