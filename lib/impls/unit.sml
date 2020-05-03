structure UnitInstances = 
struct
   type 'a m = unit

   fun append () () = ()
   fun empty () = ()
   fun invert () = ()
end

local
   structure UI : GROUP = UnitInstances 
in end