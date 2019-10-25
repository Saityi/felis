structure FunctionInstances : ARROW = 
struct
  type ('a, 'b) a = 'a -> 'b

  fun id x = x
  fun comp f g = f o g
  fun arr f = f
  fun first g (x, y) = (g x, y)
end