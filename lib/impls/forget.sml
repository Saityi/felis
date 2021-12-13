structure Forget = struct
  type ('r, 'a, 'b) forget = 'a -> 'r

  fun forget (f : 'a -> 'r) : ('r, 'a, 'b) forget =
    f
  fun runForget (f : ('r, 'a, 'b) forget) : 'a -> 'r =
    f
end
