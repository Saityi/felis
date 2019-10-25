functor OptionInstances (structure O : OPTION) : MONAD = 
struct
  type 'a m = 'a O.option

  val map = O.map

  fun pure a = O.SOME a
  fun ap (O.SOME f) m = map f m
    | ap O.NONE m  = O.NONE

  fun bind f xs = O.mapPartial f xs
end