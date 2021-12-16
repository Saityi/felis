(* This provides a set of common functions and
 * higher order functions *)
structure Base = struct
  datatype ('a, 'b) either = left of 'a
                           | right of 'b

  type 'a id = 'a
  fun takeWhile _ []        = []
    | takeWhile p (x :: xs) =
      if p x
      then x :: takeWhile p xs
      else []
  fun dropWhile _ []        = []
    | dropWhile p (x :: xs) =
      if p x
      then dropWhile p xs
      else (x :: xs)
  fun either f _ (left x) = f x
    | either _ g (right y) = g y
  fun id x = x
  fun const a b = a
  fun seq a b = let val a = a in b end
  fun uncurry f a b = f (a, b)
  fun curry f (a, b) = f a b
  fun flip f x y = f y x
  fun swap (x, y) = (y, x)
  fun flipu f = f o swap
  fun pair a b = (a, b)
  fun dupe v = (v, v)
  fun fst (a, _) = a
  fun snd (_, b) = b

  infixr 0 $
  fun f $ x = f x

  infix 9 |>
  fun x |> f = f x
end
