signature ARROW = sig
  include CATEGORY

  val arr : ('a -> 'b) -> ('a, 'b) a
  val first : ('a, 'b) a -> ('a * 'c, 'b * 'c) a
end
