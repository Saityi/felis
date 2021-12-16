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
  in
  type text = string
  (* TODO: Replace with different representation e.g. 'ParseResult' *)
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

(* Via http://dev.stephendiehl.com/fun/parsers.html *)
structure ParserCombinators = struct
  local
    open Base
    infix 9 $
    structure PM = MonadMethods(Parser)
    structure AM = AlternativeMethods(Parser)
    infix 1 >>=
    infix 6 <$> <*> <|> $> >>
  in
    open Parser AM PM
    val fail = parser (fn _ => [])
    val nextChar : char parser =
      let fun nextChar' s =
        case String.explode s of
          [] => []
        | (c::rest) => [(c, String.implode rest)]
      in parser nextChar'
      end

    fun satisfy p =
      let fun test c =
            if p c
            then pure c
            else fail
      in nextChar >>= test
      end

    fun char c =
      satisfy (fn c' => c = c')

    fun orElseP p1 p2 =
      let fun orElseP' s =
            case parse p1 s of
              [] => parse p2 s
            | xs => xs
      in parser orElseP'
      end

    fun many p =
      let val q = pure []
          val manyv =
            (p       >>= (fn pv =>
            (many p) >>= (fn pvs =>
            (pure (pv :: pvs)))))
      in manyv <|> q
      end

    fun some p =
      many p >>= (fn pvs =>
        case pvs of
          [] => fail
        | xs => pure xs)

    fun oneOf cs =
      satisfy (fn c => List.exists (fn c' => c' = c) cs)

    fun chainl1 p chain =
      let fun rest a =
        let val p1 =
              chain >>= (fn f =>
              p     >>= (fn b =>
              rest (f a b)))
            val p2 = pure a
        in p1 <|> p2
        end
      in p >>= rest
      end

    fun chainl p chain a =
      chainl1 p chain <|> pure a

    val spaces =
      many (oneOf [#"\n", #"\r", #" "])

    fun string' []        = pure []
      | string' (c :: cs) = char c >> string' cs >> (pure (c :: cs))

    fun token p =
      p >>= (fn a => spaces >> pure a)

    val digit =
      satisfy Char.isDigit

    fun string s =
      String.implode <$> string' (String.explode s)

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

    fun digitP i =
      let fun test i' =
            if i' = i
            then pure ()
            else fail
      in nextDigit >>= test
      end

    fun reserved s =
      token (string s)

    fun parens m =
      reserved "(" >>
      m >>= (fn n =>
      reserved ")" >>
      pure n)
  end
end
