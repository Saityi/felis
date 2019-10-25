# Felis

A collection of common functional abstractions, including:
- Functor
- Applicatives
- Monads
- Foldable

And instances on them, defined where possible in Basis signature terms.

```sml
λ sml
Standard ML of New Jersey v110.93 [built: Thu Sep 05 19:16:24 2019]
- CM.make "felis.cm";
- structure LF = ListInstances(structure L = List);
- structure FSyntax = FunctorSyntax(LF);
- structure FExtras = FunctorEnrichments(LF);
- open FSyntax; open FExtras; open LF;
- infix 6 <$>;
infix 6 <$>
- (fn x => x + 1) <$> [1, 2, 3];
val it = [2,3,4] : int m
- void [1, 2, 3, 4, 5];
val it = [(),(),(),(),()] : unit m
```