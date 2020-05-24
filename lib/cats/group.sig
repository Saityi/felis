signature GROUP = sig
  include MONOID
  val invert : 'a m -> 'a m
end
