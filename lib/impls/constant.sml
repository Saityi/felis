structure Constant = struct
  type ('a, 'b) constant = 'a
  fun constant (a : 'a) : ('a, 'b) constant = a
  fun getConstant (a : ('a, 'b) constant) : 'a = a
  structure Functor = struct
    fun map f a = constant (f a)
  end
end
