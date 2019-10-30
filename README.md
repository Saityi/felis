# Felis

A collection of common functional abstractions, including:
- Functor
- Applicatives
- Monads
- Foldable

And instances on them, defined where possible in Basis signature terms.

```sml
λ sml
Standard ML of New Jersey
- CM.make "felis.cm";
- structure L = ListInstances(List);
- structure LFSyntax = FunctorSyntax(L.ListMonad);
- structure LFExtras = FunctorEnrichments(L.ListMonad);
- open Base L LFSyntax LFExtras infix 6 <$>;=
- (fn x => x + 1) <$> [1, 2, 3];
val it = [2,3,4] : int m
- void [1, 2, 3, 4, 5];
val it = [(),(),(),(),()] : unit m
- ListTraversable.traverse id [[0, 1], [1, 0]];
val it = [[0,1],[0,0],[1,1],[1,0]] : int ListFoldable.m ListMonad.m
```

NOTE: Due to the lack of ad-hoc polymorphism, I fear this isn't quite as useful as it could be, but I find after a little bit of boilerplate and `open`ing a couple modules, it's more useful than I expected. Pull requests welcome!