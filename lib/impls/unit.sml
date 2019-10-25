structure UnitInstances : MONOID = 
struct
   type 'a m = unit

   fun append () () = ()
   fun empty () = ()
end