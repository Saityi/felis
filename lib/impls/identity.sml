structure IdentityInstances = struct
  local open Base in
    structure Monad : MONAD = struct
      type 'a m = 'a id
      fun map f v = f v
      fun pure v = v
      val ap   = map
      val bind = ap
    end
  end
end
