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
structure L :
  sig
    structure Monoid : <sig>
    structure Monad : <sig>
    structure Alternative : <sig>
    structure Foldable : <sig>
    structure Traversable : <sig>
  end

- structure LFMethods = FunctorMethods(L.Monad);
structure LFMethods :
  sig
    val <$> : ('a -> 'b) * 'a F.m -> 'b F.m
    val $> : 'a F.m * 'b -> 'b F.m
    val <$ : 'a * 'b F.m -> 'a F.m
    val <&> : 'a F.m * ('a -> 'b) -> 'b F.m
    val void : 'a F.m -> unit F.m
  end

- open Base L LFMethods infix 6 <$>;
opening Base
  datatype ('a,'b) either = left of 'a | right of 'b
  type 'a id = {unid:'a}
  type ('a,'b) const = {unconst:'a}
  val id : 'a -> 'a
  val const : 'a -> 'b -> 'a
  val seq : 'a -> 'b -> 'b
  val uncurry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
  val curry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  val swap : 'a * 'b -> 'b * 'a
  val flipu : ('a * 'b -> 'c) -> 'b * 'a -> 'c
  val pair : 'a -> 'b -> 'a * 'b
  val dupe : 'a -> 'a * 'a
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
  val $ : ('a -> 'b) * 'a -> 'b
  val |> : 'a * ('a -> 'b) -> 'b

opening L
  structure Monoid :
    sig
      type 'a m = 'a list
      val append : 'a m -> 'a m -> 'a m
      val empty : unit -> 'a m
    end
  structure Monad :
    sig
      type 'a m = 'a list
      val map : ('a -> 'b) -> 'a m -> 'b m
      val pure : 'a -> 'a m
      val ap : ('a -> 'b) m -> 'a m -> 'b m
      val bind : ('a -> 'b m) -> 'a m -> 'b m
    end
  structure Alternative :
    sig
      type 'a m = 'a list
      val map : ('a -> 'b) -> 'a m -> 'b m
      val pure : 'a -> 'a m
      val ap : ('a -> 'b) m -> 'a m -> 'b m
      val empty : unit -> 'a m
      val alt : 'a m -> 'a m -> 'a m
    end
  structure Foldable :
    sig
      type 'a m = 'a list
      val foldr : ('a -> 'b -> 'b) -> 'b -> 'a m -> 'b
    end
  structure Traversable :
    sig
      structure A : <sig>
      structure F : <sig>
      val traverse : ('a -> 'b A.m) -> 'a F.m -> 'b F.m A.m
    end

opening LFMethods
  val <$> : ('a -> 'b) * 'a F.m -> 'b F.m
  val $> : 'a F.m * 'b -> 'b F.m
  val <$ : 'a * 'b F.m -> 'a F.m
  val <&> : 'a F.m * ('a -> 'b) -> 'b F.m
  val void : 'a F.m -> unit F.m

infix 6 <$>

- (fn x => x + 1) <$> [1, 2, 3];
val it = [2,3,4] : int Monad.m

- void [1, 2, 3, 4, 5];
val it = [(),(),(),(),()] : unit Monad.m

- L.Traversable.traverse id [[0, 1], [1, 0]];
val it = [[0,1],[0,0],[1,1],[1,0]] : int Foldable.m Monad.m
```

NOTE: Due to the lack of ad-hoc polymorphism, I fear this isn't quite as useful as it could be, but I find after a little bit of boilerplate and `open`ing a couple modules, it's more useful than I expected. Pull requests welcome!
