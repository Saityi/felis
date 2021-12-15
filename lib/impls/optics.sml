(* Via https://github.com/hablapps/DontFearTheProfunctorOptics/blob/master/Optics.md *)
structure Optics = struct
  structure Concrete = struct
    local
      structure OptionInsts   = OptionInstances(Option)
      structure OptionMethods = FunctorMethods(OptionInsts.Monad)
      open OptionInsts OptionMethods
      infix 4 $>
    in
      datatype ('s, 't, 'a, 'b) lens =
        lens of { view   : 's -> 'a
                , update : ('b * 's) -> 't
                }
      fun update (lens{view=_,update=u}) = u
      fun view (lens{view=v,update=_}) = v
      (* p1, shift unusable when instantiated to dummy variables
        as values E.g.
        - from shift ((1,2),3);
        stdIn:66.1-66.21 Error: operator and operand don't agree
        [overload conflict]
        - from (shift ()) ((1,2),3);
        (1,(2,3))
      *)
      fun p1 () =
        let fun fst (a, b) = a
            fun u (b,(_,c)) = (b, c)
        in lens { view = fst, update = u }
        end

      datatype ('s, 't, 'a, 'b) adapter =
        adapter of { from : 's -> 'a
                  , to : 'b -> 't
                  }
      fun from (adapter{from=f,to=t}) = f
      fun to (adapter{from=f,to=t}) = t
      fun shift () =
        let fun f ((a, b), c) = (a, (b, c))
            fun t (a, (b, c)) = ((a, b), c)
        in adapter { from = f, to = t }
        end

      datatype ('s, 't, 'a, 'b) prism =
        prism of { match : 's -> ('a, 't) Base.either
                , build : 'b -> 't
                }

      fun match (prism{match=m,build=b}) = m
      fun build (prism{match=m,build=b}) = b
      fun maybe n _ NONE     = n
        | maybe _ f (SOME v) = f v
      fun the () =
          let fun r m = maybe (Base.right NONE) Base.left m
          in prism { match = r, build = SOME }
          end

      datatype ('s, 't, 'a, 'b) affine =
        affine of { preview : 's -> ('a, 't) Base.either
                  , set     : ('b * 's) -> 't
                  }
      fun preview (affine{preview=p, set=s}) = p
      fun set (affine{preview=p, set=s})     = s
      fun maybeFirst () =
        let fun p (ma, c) = maybe (Base.right (NONE, c)) Base.left ma
            fun st (b, (ma, c)) = (ma $> b, c)
        in affine {preview=p, set=st}
        end

      datatype ('s, 't, 'a, 'b) traversal =
        traversal of { contents : 's -> 'a list
                    , fill     : 'b list -> 's -> 't
                    }
      fun contents (traversal{contents=c,fill=f}) = c
      fun fill (traversal{contents=c,fill=f}) = f
      fun firstNSecond () =
        let fun c (a1,a2,_) = [a1, a2]
            fun f bs (_,_,x) = (hd bs, (hd o tl) bs, x)
        in traversal { contents=c, fill=f }
        end
    end (* END local *)
  end (* END struct Concrete *)

  (* Via https://github.com/solomon-b/profunctor-optics/blob/master/src/Data/Optics.hs*)
  (* https://r6research.livejournal.com/27476.html *)
  functor Adapter (structure P : PROFUNCTOR) = struct
    local
      open Base
      structure T  = Tagged
      structure F  = Forget
      structure C  = Constant
      infix 0 $
    in
      type ('s, 't, 'a, 'b) adapter = ('a, 'b) P.a -> ('s, 't) P.a
      fun adapter (f : 's -> 'a) (g : 'b -> 't) : ('s, 't, 'a, 'b) adapter =
        P.dimap f g
      fun from adapt s =
        (F.runForget o adapt $ F.forget id) s
      fun to adapt b =
        (T.unTagged o adapt o T.tagged) b
      fun shift () =
        let fun f ((a, b), c) = (a, (b, c))
            fun t (a, (b, c)) = ((a, b), c)
        in adapter f t
        end
      fun adapterC2P (Concrete.adapter{from=f, to=t}) : ('s, 't, 'a, 'b) adapter = P.dimap f t
    end
  end

  functor Lens (structure P : CARTESIAN) = struct
    local
      open Base
      structure T  = Tagged
      structure F  = Forget
      structure C  = Constant
      infix 0 $
      structure PM = ProfunctorMethods(P)
    in
      type ('s, 't, 'a, 'b) lens = ('a, 'b) P.a -> ('s, 't) P.a
      fun lens' to l = P.dimap to (fn (b, f) => f b) (P.first l)
      fun lens get set = lens' $ (fn s => (get s, set s))
      fun one () =
        let val f = fst
            fun g (_, c) b = (b, c)
        in lens f g
        end
      fun two () =
        let val f = snd
            fun g (c, _) b = (c, b)
        in lens f g
        end
      fun p1 () = P.first
      fun p2 () = P.second
      fun matching sca cbt = P.dimap sca cbt o P.second
      fun lensC2P (Concrete.lens{view=v,update=u}) =
          let
            fun dup a = (a, a)
          in
            P.dimap dup u o P.first o PM.lmap v
          end
      end
    end


end
