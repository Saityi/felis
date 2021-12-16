signature PARSER = sig
  type 'a parser
  type text
  val runParser : 'a parser -> (text -> ('a * text) list)
  val parse : 'a parser -> text -> ('a * text) list
end

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
  type text = string
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
structure ParserParser : PARSER = Parser
structure ParserMonad : MONAD = Parser
structure ParserAlternative : ALTERNATIVE = Parser

structure ParserCombinators = struct
  local
    open Base
    infix 9 $
    structure PM = MonadMethods(Parser)
    structure AM = AlternativeMethods(Parser)
    infixr 1 >>=
    infix 6 <$> <*> <|> $>
    fun cons x xs = x :: xs
  in
    open Parser AM PM
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

    (* Parse as large of an int as possible from the head of a char stream *)
    val intP =
      let fun intP' s =
        let val cx     = String.explode s
            val digits = takeWhile Char.isDigit cx
            val rest   = dropWhile Char.isDigit cx
        in case Int.fromString (String.implode digits) of
             SOME i => [(i, String.implode rest)]
           | NONE   => []
        end
      in parser intP'
      end

    val nextDigit =
      let fun digitP' s =
        case String.explode s of
          (c :: cx') =>
            (case Int.fromString (Char.toString c) of
              SOME i => [(i, String.implode cx')]
             | NONE   => [])
        | []      => []
      in parser digitP'
      end

    fun digitP i = nextDigit >>= (fn i' => if i' = i then pure () else noParserP)
  end
end
