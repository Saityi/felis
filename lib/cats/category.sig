signature CATEGORY = sig
  type ('a, 'b) a

  val id : ('a, 'a) a
  val comp  : ('b, 'c) a -> ('a, 'b) a -> ('a, 'c) a
end
