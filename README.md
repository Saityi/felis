# Felis

A collection of common functional abstractions, including:
- Functor
- Applicatives
- Monads
- Foldable

And instances on them, defined where possible in Basis signature terms.

Include a small module of utility functions, tentatively named `Base`. Includes the types `id` and `either`, as well as utility functions like `id`, `const`, `uncurry`, `curry`, and `flip`, among others.

```sml
λ sml
Standard ML of New Jersey
- CM.make "felis.cm";
(* ListInstances is defined using the LIST signature from the standard library *)
(* Here, we instantiate it using SML/NJ's List structure *)
- structure L = ListInstances(List); 
structure L :
  sig
    structure Monoid : <sig>
    structure Monad : <sig>
    structure Alternative : <sig>
    structure Foldable : <sig>
    functor Traversable : <fctsig>
  end

- structure LT = L.Traversable(L.Monad);
structure LT : TRAVERSABLE

- structure LF = FunctorMethods(L.Monad);
structure LF :
  sig
    val <$> : ('a -> 'b) * 'a F.m -> 'b F.m
    val $> : 'a F.m * 'b -> 'b F.m
    val <$ : 'a * 'b F.m -> 'a F.m
    val <&> : 'a F.m * ('a -> 'b) -> 'b F.m
    val void : 'a F.m -> unit F.m
  end

- open Base L LF;
- infix 6 <$>;
- (fn x => x + 1) <$> [1, 2, 3];
val it = [2,3,4] : int Monad.m

- void [1, 2, 3, 4, 5];
val it = [(),(),(),(),()] : unit Monad.m

- LT.traverse id [[0, 1], [1, 0]];
val it = [[0,1],[0,0],[1,1],[1,0]] : int Foldable.m Monad.m
```

NOTE: Due to the lack of ad-hoc polymorphism, I fear this isn't quite as useful as it could be, but I find after a little bit of boilerplate and `open`ing a couple modules, it's more useful than I expected. Pull requests welcome!
