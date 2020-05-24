functor StringInstances (S : STRING) : MONOID = struct
  local
    open S Base
  in
    type 'a m = string

    val append = uncurry op ^
    fun empty () = implode []
  end
end
