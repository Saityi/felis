(* This provides a set of common functions and
 * higher order functions *)
structure Base = 
struct
  datatype ('a, 'b) either = left of 'a
                           | right of 'b

  type 'a id = { unid : 'a }
  type ('a, 'b) const = { unconst : 'a }

  fun id x = x
  fun const a b = a
  fun uncurry f a b = f (a, b)
  fun curry f (a, b) = f a b
  fun flip f x y = f y x
  fun swap (x, y) = (y, x)
  fun flipu f = f o swap
  fun pair a b = (a, b)
  fun dupe v = (v, v)

  infixr 0 $
  fun f $ x = f x

  infix 9 |>
  fun x |> f = f x
end