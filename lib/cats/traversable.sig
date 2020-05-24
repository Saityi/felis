signature TRAVERSABLE = sig
   structure A : APPLICATIVE
   structure F : FOLDABLE

   val traverse : ('a -> 'b A.m) -> 'a F.m -> ('b F.m) A.m
end
