signature SEMIGROUP = sig
  type 'a m

  val append : 'a m -> 'a m -> 'a m
end
