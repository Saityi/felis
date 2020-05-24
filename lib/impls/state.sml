functor StateMonad (type s) : MONAD = struct
  open Base
  infix 0 $
  datatype 'a m = state of { runState : s -> (s * 'a) }
  structure M = TupleFInstances (type b = s)
  structure F = M.Functor

  fun map f (state {runState=g}) =
    state { runState = fn s => F.map f (g s) }

  fun pure v =
    state { runState = fn s => (s, v) }

  fun ap (state {runState=fs}) (state {runState=vs}) =
    state { runState = fn s =>
      let
        val (s', v) = vs s
        val (s'', f) = fs s'
      in
        (s'', f v)
      end
    }

  fun bind k (state {runState=q}) =
    state { runState = fn s =>
      let
        val (s', v) = q s
        val (state {runState=q'}) = k v
      in
        q' s'
      end
    }
end
