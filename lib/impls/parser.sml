structure Parser = struct
  local
    open Base
    infix 9 $
    structure LI = ListInstances(List)
    structure LM = MonadMethods(LI.Monad)
    structure LS = MonadSyntax(LI.Monad)
    open LM LS
    infix 1 >>=
    infix 6 <$> <*>
    fun concatMap (f : 'a -> 'b list) (xs : 'a list) : 'b list =
      List.foldr (fn (x, acc) => acc @ f x) [] xs
  in
  datatype 'a parser = parser of string -> ('a * string) list
  type 'a m = 'a parser

  fun runParser (parser p) : string -> ('a * string) list = p
  fun parse (p : 'a parser) (s : string) : ('a * string) list = (runParser p) s

  fun map f p =
    parser (fn s => case parse p s of
                      [] => []
                    | xs => List.map (fn (a, s) => (f a, s)) xs)

  fun pure v = parser (fn s => [(v, s)])

  fun ap (parser pf) (parser pa)  =
    let fun ap' s = (pf s)  >>= (fn (f, s')  =>
                    (pa s') >>= (fn (a, s'') =>
                    LI.Monad.pure (f a, s'')))
    in parser ap'
    end

  fun bind (f : 'a -> 'b parser) (p : 'a parser) : 'b parser =
    let fun bind' s = concatMap (fn (a, s') => parse (f a) s') (parse p s)
    in parser bind'
    end
  end

  fun combine p q = parser (fn s => parse p s @ parse q s)
  fun empty () = parser (fn s => [])
  fun alt pa pb = parser (fn s => case parse (combine pa pb) s of
                                    []      => []
                                  | (x::xs) => [x])
end

structure ParserMonad : MONAD = Parser
structure ParserAlternative : ALTERNATIVE = Parser

structure ParserCombinators = struct
  local
    open Base
    infix 9 $
    structure PM = MonadMethods(Parser)
    structure PS = MonadSyntax(Parser)
    structure AM = AlternativeMethods(Parser)
    structure AS = AlternativeSyntax(Parser)
    infixr 1 >>=
    infix 6 <$> <*> <|>
    fun cons x xs = x :: xs
  in
    open Parser AM AS PM PS
    val anyChar : char parser =
      let fun anyChar' s =
        case String.explode s of
          [] => []
        | (c::rest) => [(c, String.implode rest)]
      in parser anyChar'
      end
    val noParserP = parser (fn _ => [])
    fun charP c =
      anyChar >>= (fn c' =>
        if c = c'
        then pure ()
        else noParserP)
    infix 6 orElseP
    fun p1 orElseP p2 =
      let fun orElseP' s = case parse p1 s of
                             [] => parse p2 s
                           | xs => xs
      in parser orElseP'
      end
    fun manyP p =
      (pure (uncurry op ::) <*> p <*> manyP p) orElseP (pure [])

  end
end
