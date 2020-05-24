signature MONOID = sig
  include SEMIGROUP

  val empty : unit -> 'a m
end
