signature SEMIGROUP = sig
  include HKT

  val append : 'a m -> 'a m -> 'a m
end
