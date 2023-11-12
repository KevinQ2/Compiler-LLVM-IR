import ammonite.ops._

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class RECD(x: String, r: Rexp) extends Rexp
case class RANGE(c: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val

// some convenience for typing regular expressions

def charlist2rexp(s: List[Char]): Rexp = s match {
    case Nil => ONE
    case c::Nil => CHAR(c)
    case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

// produces a rexp from a string when expected a Rexp
implicit def string2rexp(s: String) : Rexp = charlist2rexp(s.toList)

// rexp op argument
implicit def RexpOps(r: Rexp) = new {
    def | (s: Rexp) = ALT(r, s)
    def % = STAR(r)
    def ~ (s: Rexp) = SEQ(r, s)
}

// string op argument
implicit def stringOps(s: String) = new {
    def | (r: Rexp) = ALT(s, r)
    def | (r: String) = ALT(s, r)
    def % = STAR(s)
    def ~ (r: Rexp) = SEQ(s, r)
    def ~ (r: String) = SEQ(s, r)
    def $ (r: Rexp) = RECD(s, r)
}

// nullable and der
def nullable(r: Rexp) : Boolean = r match {
    case ZERO => false
    case ONE => true
    case CHAR(_) => false
    case ALT(r1, r2) => nullable(r1) || nullable(r2)
    case SEQ(r1, r2) => nullable(r1) && nullable(r2)
    case STAR(_) => true
    case RECD(_, r1) => nullable(r1)
    case RANGE(r1) => r1.isEmpty
    case PLUS(r1) => nullable(r1)
    case OPTIONAL(_) => true
    case NTIMES(r1, n) => nullable(r1) || n == 0
}

def der(c: Char, r: Rexp) : Rexp = r match {
    case ZERO => ZERO
    case ONE => ZERO
    case CHAR(d) => if (c == d) ONE else ZERO
    case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
    case SEQ(r1, r2) => {
        if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
        else SEQ(der(c, r1), r2)
    }
    case STAR(r1) => SEQ(der(c, r1), STAR(r1))
    case RECD(_, r1) => der(c, r1)
    case RANGE(r1) => {
        if (r1.contains(c)) ONE
        else ZERO
    }
    case PLUS(r1) => SEQ(der(c, r1), STAR(r1))
    case OPTIONAL(r1) => der(c, r1)
    case NTIMES(r1, n) => {
        if (n <= 0) ZERO
        else SEQ(der(c, r1), NTIMES(r1, n-1))
    }
}

// extracts a string from a value
def flatten(v: Val) : String = v match {
    case Empty => ""
    case Chr(c) => c.toString
    case Left(v1) => flatten(v1)
    case Right(v1) => flatten(v1)
    case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
    case Stars(vs) => vs.map(flatten).mkString
    case Rec(_, v1) => flatten(v1)
}


// extracts an environment from a value
// used for tokenising a string
def env(v: Val) : List[(String, String)] = v match {
    case Empty => Nil
    case Chr(c) => Nil
    case Left(v1) => env(v1)
    case Right(v1) => env(v1)
    case Sequ(v1, v2) => env(v1) ::: env(v2)
    case Stars(vs) => vs.flatMap(env)
    case Rec(x, v1) => (x, flatten(v1))::env(v1)
}


// r must be nullable, it finds the value of how it can match the empty string
def mkeps(r: Rexp) : Val = r match {
    case ONE => Empty
    case ALT(r1, r2) => {
        if (nullable(r1)) Left(mkeps(r1))
        else Right(mkeps(r2))
    }
    case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
    case STAR(_) => Stars(Nil)
    case RECD(x, r1) => Rec(x, mkeps(r1))
    case PLUS(r) => Stars(List(mkeps(r)))
    case OPTIONAL(r) => Stars(Nil)
    case NTIMES(r, n) => Stars(List.fill(n)(mkeps(r)))
}

// assuming r matches the string, it reverts the derivation
def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
    case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
    case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1) , inj(r2, c, v2))
    case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
    case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
    case (CHAR(d), Empty) => Chr(c)
    case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))
    case (RANGE(_), Empty) => Chr(c)
    case (PLUS(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
    case (OPTIONAL(r), v) => Stars(inj(r, c, v)::Nil)
    case (NTIMES(r, n), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
}

// some "rectification" functions for simplification
def F_ID(v: Val): Val = v
def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))
def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))
def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Right(v) => Right(f2(v))
  case Left(v) => Left(f1(v))
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) = 
  (v:Val) => Sequ(f1(Empty), f2(v))
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) = 
  (v:Val) => Sequ(f1(v), f2(Empty))

def F_ERROR(v: Val): Val = throw new Exception("error")

// simplification
def simp(r: Rexp): (Rexp, Val => Val) = r match {
  case ALT(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (r2s, F_RIGHT(f2s))
      case (_, ZERO) => (r1s, F_LEFT(f1s))
      case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
                else (ALT (r1s, r2s), F_ALT(f1s, f2s)) 
    }
  }
  case SEQ(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (ZERO, F_ERROR)
      case (_, ZERO) => (ZERO, F_ERROR)
      case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
      case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
      case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
    }
  }
  case r => (r, F_ID)
}

// lexing functions without simplification
def lex(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => if (nullable(r)) mkeps(r) else 
    { throw new Exception("lexing error") } 
  case c::cs => inj(r, c, lex(der(c, r), cs))
}

def lexing(r: Rexp, s: String) = 
  env(lex(r, s.toList))

// lexing functions including simplification
def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => 
    if (nullable(r)) mkeps(r)
    else { throw new Exception("lexing error") } 
  case c::cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

def lexing_simp(r: Rexp, s: String) = env(lex_simp(r, s.toList))


// The Lexing Rules for the WHILE Language

def PLUSL(r: Rexp) = r ~ r.%

def Range(s: List[Char]) : Rexp = s match {
    case Nil => ZERO
    case c::Nil => CHAR(c)
    case c::d => RANGE(s.toSet)
}

def RANGEL(s: String) = Range(s.toList)

val UPPERLETTER = RANGEL("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
val LOWERLETTER = RANGEL("abcdefghijklmnopqrstuvwxyz")
val LETTER = UPPERLETTER | LOWERLETTER
val DIGIT = RANGEL("0123456789")

val ID = LOWERLETTER ~ ("_" | LETTER | DIGIT).%
val CONSTANT = UPPERLETTER ~ ("_" | LETTER | DIGIT).%
val NUM = (ONE | "-") ~ DIGIT | (RANGEL("123456789") ~ PLUSL(DIGIT))
val FLOAT = NUM ~ "." ~ PLUSL(DIGIT)

val KEYWORD : Rexp = "val" | "def" | "skip" |
                    "print_int" | "print_char" | "print_star" | "print_space" | "new_line" |
                    "if" | "then" | "else"
val TYPE : Rexp = "Int" | "Double" | "Void"
val SEMI: Rexp = ";"
val COLON: Rexp = ":"
val COMMA: Rexp = ","
val OP: Rexp = "=" | "+" | "-" | "*" | "%" | "/" | "==" | "!=" | ">" | "<" | "<=" | ">=" | "&&" | "||"
val WHITESPACE = PLUSL(" " | "\n" | "\t" | "\r")
val WHITESPACECHAR = " " | "\\n" | "\\t" | "\\r"
val RPAREN: Rexp = "}" | ")"
val LPAREN: Rexp = "{" | "("

val ALL = LETTER | DIGIT | WHITESPACECHAR | RANGEL("+-*/%<>=&|(){}.,_;:") | "\\" | "\'" | "\""
val ALL2 = ALL | WHITESPACE

val STRING: Rexp = "\"" ~ ALL2.% ~ "\""
val CHARACTER: Rexp = "\'" ~ ALL ~ "\'"

val COMMENT = ("/*" ~ ALL2.% ~ "*/") | ("//" ~ ALL.% ~ "\n")

val FUN_REGS =   (("k" $ KEYWORD) |
                    ("t" $ TYPE) |
                    ("i" $ ID) |
                    ("constant" $ CONSTANT) |
                    ("o" $ OP) |
                    ("f" $ FLOAT) |
                    ("n" $ NUM) |
                    ("s" $ SEMI) |
                    ("colon" $ COLON) |
                    ("comma" $ COMMA) |
                    ("str" $ STRING) |
                    ("char" $ CHARACTER) |
                    ("p" $ (LPAREN | RPAREN)) |
                    ("w" $ WHITESPACE) |
                    ("c" $ COMMENT)).%


// The tokens for the WHILE language
abstract class Token 
case object T_SEMI extends Token
case object T_COLON extends Token
case object T_COMMA extends Token
case object T_LPAREN extends Token
case object T_RPAREN extends Token
case object T_CLPAREN extends Token
case object T_CRPAREN extends Token
case class T_ID(s: String) extends Token
case class T_CONSTANT(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_FLOAT(n: Float) extends Token
case class T_KWD(s: String) extends Token
case class T_TYPE(s: String) extends Token
case class T_STR(s: String) extends Token
case class T_CHAR(n: Int) extends Token

// Can also filter unwanted tokens (e.g. whitespace)
val token : PartialFunction[(String, String), Token] = {
  case ("s", _) => T_SEMI
  case ("colon", _) => T_COLON
  case ("comma", _) => T_COMMA
  case ("p", "{") => T_CLPAREN
  case ("p", "(") => T_LPAREN
  case ("p", "}") => T_CRPAREN
  case ("p", ")") => T_RPAREN
  case ("i", s) => T_ID(s)
  case ("constant", s) => T_CONSTANT(s)
  case ("o", s) => T_OP(s)
  case ("n", s) => T_NUM(s.toInt)
  case ("f", s) => T_FLOAT(s.toFloat)
  case ("k", s) => T_KWD(s)
  case ("t", s) => T_TYPE(s)
  case ("str", s) => T_STR(StringContext processEscapes s.substring(1, s.length() - 1)) // de-escapes strings
  case ("char", s) => T_CHAR((StringContext processEscapes s.substring(1, s.length() - 1)).head.toInt) // converts to ascii value
}

// by using collect we filter out all unwanted tokens
def tokenise(s: String) : List[Token] = lexing_simp(FUN_REGS, s).collect(token)

// parser combinators
abstract class Parser[I, T](implicit ev: I => Seq[_]) {
  def parse(ts: I): Set[(T, I)]

  def parse_single(ts: I) : T = {
    parse(ts).partition(_._2.isEmpty) match {
      case (good, _) if !good.isEmpty => good.head._1
      case (_, err) => { 
	println (s"Parse Error\n${err.minBy(_._2.length)}") ; sys.exit(-1) }
    }
  }
}

// convenience for writing grammar rules
case class ~[+A, +B](_1: A, _2: B)

class SeqParser[I, T, S](p: => Parser[I, T], 
                         q: => Parser[I, S])(implicit ev: I => Seq[_]) extends Parser[I, ~[T, S]] {
  def parse(sb: I) = 
    for ((head1, tail1) <- p.parse(sb); 
         (head2, tail2) <- q.parse(tail1)) yield (new ~(head1, head2), tail2)
}

class AltParser[I, T](p: => Parser[I, T], 
                      q: => Parser[I, T])(implicit ev: I => Seq[_]) extends Parser[I, T] {
  def parse(sb: I) = p.parse(sb) ++ q.parse(sb)   
}

class FunParser[I, T, S](p: => Parser[I, T], 
                         f: T => S)(implicit ev: I => Seq[_]) extends Parser[I, S] {
  def parse(sb: I) = 
    for ((head, tail) <- p.parse(sb)) yield (f(head), tail)
}

// convenient combinators
implicit def ParserOps[I, T](p: Parser[I, T])(implicit ev: I => Seq[_]) = new {
  def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
}

// parser for lists
def ListParser[I, T, S](p: => Parser[I, T], 
                        q: => Parser[I, S])(implicit ev: I => Seq[_]): Parser[I, List[T]] = {
  (p ~ q ~ ListParser(p, q)) ==> { case x ~ _ ~ z => x :: z : List[T] } ||
  (p ==> ((s) => List(s)))
}

// atomic parser for (particular) tokens
case class TokenParser(t: Token) extends Parser[List[Token], Token] {
  def parse(tb: List[Token]) = {
    if (tb.length != 0){
      if (tb.head == t) Set((tb.head, tb.tail)) else Set()
    }
    else Set()
  }
}

implicit def token2tparser(t: Token) = TokenParser(t)

implicit def TokOps(t: Token) = new {
  def || (q : => Parser[List[Token], Token]) = new AltParser[List[Token], Token](t, q)
  def ==>[S] (f: => Token => S) = new FunParser[List[Token], Token, S](t, f)
  def ~[S](q : => Parser[List[Token], S]) = new SeqParser[List[Token], Token, S](t, q)
}

// atomic parser for strings
case object StrParser extends Parser[List[Token], String] {
  def parse(tb: List[Token]) = {
    if (tb.length != 0) tb.head match {
      case T_STR(s) => {
        Set((s, tb.tail)) 
      }
      case _ => Set()
    }
    else Set()
  }
}

// atomic parser for identifier
case object IdParser extends Parser[List[Token], String] {
  def parse(tb: List[Token]) = {
    if (tb.length != 0) tb.head match {
      case T_ID(s) => {
        Set((s, tb.tail)) 
      }
      case _ => Set()
    }
    else Set()
  }
}

// atomic parser for global constants
case object ConstantParser extends Parser[List[Token], String] {
  def parse(tb: List[Token]) = {
    if (tb.length != 0) tb.head match {
      case T_CONSTANT(s) => {
        Set((s, tb.tail)) 
      }
      case _ => Set()
    }
    else Set()
  }
}

// atomic parser for numbers
case object NumParser extends Parser[List[Token], Int] {
  def parse(tb: List[Token]) = {
    if (tb.length != 0) tb.head match {
      case T_NUM(n) => {
        Set((n, tb.tail)) 
      }
      case _ => Set()
    }
    else Set()
  }
}

// atomic parser for doubles
case object FloatParser extends Parser[List[Token], Float] {
  def parse(tb: List[Token]) = {
    if (tb.length != 0) tb.head match {
      case T_FLOAT(n) => {
        Set((n, tb.tail)) 
      }
      case _ => Set()
    }
    else Set()
  }
}

// atomic parser for characters
case object CharParser extends Parser[List[Token], Int] {
  def parse(tb: List[Token]) = {
    if (tb.length != 0) tb.head match {
      case T_CHAR(n) => {
        Set((n, tb.tail)) 
      }
      case _ => Set()
    }
    else Set()
  }
}

// parser for argument types (int, double)
case object InTypeParser extends Parser[List[Token], String] {
  def parse(tb: List[Token]) = {
    if (tb.length != 0) tb.head match {
      case T_TYPE("Int") => {
        Set(("Int", tb.tail)) 
      }
      case T_TYPE("Double") => {
        Set(("Double", tb.tail)) 
      }
      case _ => Set()
    }
    else Set()
  }
}

// parser for types (int, double, void)
case object TypeParser extends Parser[List[Token], String] {
  def parse(tb: List[Token]) = {
    if (tb.length != 0) tb.head match {
      case T_TYPE(s) => {
        Set((s, tb.tail)) 
      }
      case _ => Set()
    }
    else Set()
  }
}

// the abstract syntax trees for the WHILE language
abstract class Exp
abstract class BExp 
abstract class Decl

case class Def(name: String, args: List[(String, String)], ty: String, body: Exp) extends Decl
case class Main(e: Exp) extends Decl
case class Const(name: String, v: Int) extends Decl
case class FConst(name: String, x: Float) extends Decl

case object Skip extends Exp
case object WriteLine extends Exp
case object WriteStar extends Exp
case object WriteSpace extends Exp
case class WriteInt(e: Exp) extends Exp
case class WriteChar(e: Exp) extends Exp
case class Call(name: String, args: List[Exp]) extends Exp
case class If(a: BExp, e1: Exp, e2: Exp) extends Exp
case class Var(s: String) extends Exp
case class ConstVal(s: String) extends Exp
case class Num(i: Int) extends Exp  // integer numbers
case class FNum(i: Float) extends Exp // floating numbers
case class ChConst(c: Int) extends Exp  // char constants
case class Aop(o: String, a1: Exp, a2: Exp) extends Exp
case class Sequence(e1: Exp, e2: Exp) extends Exp
case class Bop(o: String, a1: Exp, a2: Exp) extends BExp


// Grammar Rules for the Fun language

// arithmetic expressions
lazy val Exp: Parser[List[Token], Exp] = 
  (T_CLPAREN ~ Exp ~ T_CRPAREN) ==> { case _ ~ x ~ _ => x: Exp } ||
  (T_KWD("if") ~ BExp ~ T_KWD("then") ~ Exp ~ T_KWD("else") ~ Exp) ==>
    { case _ ~ x ~ _ ~ y ~ _ ~ z => If(x, y, z): Exp } ||
  (M ~ T_SEMI ~ Exp) ==> { case x ~ _ ~ y => Sequence(x, y): Exp } || M
lazy val M: Parser[List[Token], Exp] =
  (T_KWD("print_int") ~ T_LPAREN ~ L ~ T_RPAREN) ==> { case _ ~ _ ~ y ~ _ => WriteInt(y): Exp } ||
  (T_KWD("print_char") ~ T_LPAREN ~ L ~ T_RPAREN) ==> { case _ ~ _ ~ y ~ _ => WriteChar(y): Exp } ||
  (N ~ T_LPAREN ~ T_RPAREN) ==> { case x ~ _ ~ _ => x: Exp } || N || L
lazy val N: Parser[List[Token], Exp] =
  T_KWD("skip") ==> {case _ => Skip: Exp } ||
  T_KWD("print_star") ==> { case _ => WriteStar: Exp } ||
  T_KWD("print_space") ==> { case _ => WriteSpace: Exp } ||
  T_KWD("new_line") ==> { case _ => WriteLine: Exp }
lazy val L: Parser[List[Token], Exp] = 
  (T ~ T_OP("+") ~ Exp) ==> { case x ~ _ ~ z => Aop("+", x, z): Exp } ||
  (T ~ T_OP("-") ~ Exp) ==> { case x ~ _ ~ z => Aop("-", x, z): Exp } || T  
lazy val T: Parser[List[Token], Exp] = 
  (F ~ T_OP("*") ~ T) ==> { case x ~ _ ~ z => Aop("*", x, z): Exp } || 
  (F ~ T_OP("/") ~ T) ==> { case x ~ _ ~ z => Aop("/", x, z): Exp } || 
  (F ~ T_OP("%") ~ T) ==> { case x ~ _ ~ z => Aop("%", x, z): Exp } || F
lazy val F: Parser[List[Token], Exp] = 
  (IdParser ~ T_LPAREN ~ ListParser(Exp, T_COMMA) ~ T_RPAREN) ==> 
    { case x ~ _ ~ z ~ _ => Call(x, z): Exp } ||
  (IdParser ~ T_LPAREN ~ T_RPAREN) ==> 
    { case x ~ _ ~ _ => Call(x, List()): Exp } ||
  (T_LPAREN ~ Exp ~ T_RPAREN) ==> { case _ ~ y ~ _ => y: Exp } || 
  IdParser ==> { case x => Var(x): Exp } ||
  ConstantParser ==> { case x => ConstVal(x): Exp } || 
  NumParser ==> { case x => Num(x): Exp } ||
  FloatParser ==> { case x => FNum(x): Exp } ||
  CharParser ==> { case x => ChConst(x): Exp }
lazy val Arg: Parser[List[Token], (String, String)] = 
  (IdParser ~ T_COLON ~ InTypeParser) ==> { case x ~ _ ~ z => (x, z): (String, String)}

// boolean expressions
lazy val BExp: Parser[List[Token], BExp] = 
  (Exp ~ T_OP("==") ~ Exp) ==> { case x ~ _ ~ z => Bop("==", x, z): BExp } || 
  (Exp ~ T_OP("!=") ~ Exp) ==> { case x ~ _ ~ z => Bop("!=", x, z): BExp } || 
  (Exp ~ T_OP("<") ~ Exp)  ==> { case x ~ _ ~ z => Bop("<",  x, z): BExp } || 
  (Exp ~ T_OP(">") ~ Exp)  ==> { case x ~ _ ~ z => Bop("<",  z, x): BExp } || 
  (Exp ~ T_OP("<=") ~ Exp) ==> { case x ~ _ ~ z => Bop("<=", x, z): BExp } || 
  (Exp ~ T_OP("=>") ~ Exp) ==> { case x ~ _ ~ z => Bop("<=", z, x): BExp } ||
  (T_LPAREN ~ BExp ~ T_RPAREN) ==> { case _ ~ x ~ _ => x: BExp}

lazy val Defn: Parser[List[Token], Decl] =
  (T_KWD("def") ~ IdParser ~ T_LPAREN ~ ListParser(Arg, T_COMMA) ~ T_RPAREN ~ T_COLON ~ TypeParser ~ T_OP("=") ~ Exp) ==> 
    { case _ ~ id ~ _ ~ args ~ _ ~ _ ~ ty ~ _ ~ bo => Def(id, args, ty, bo): Decl } ||
  (T_KWD("def") ~ IdParser ~ T_LPAREN ~ T_RPAREN ~ T_COLON ~ TypeParser ~ T_OP("=") ~ Exp) ==> 
    { case _ ~ id ~ _ ~ _ ~ _ ~ ty ~ _ ~ bo => Def(id, List(), ty, bo): Decl } ||
  (T_KWD("val") ~ ConstantParser ~ T_COLON ~ T_TYPE("Int") ~ T_OP("=") ~ NumParser) ==> { case _ ~ x ~ _ ~ _ ~ _ ~ z => Const(x, z): Decl } ||
  (T_KWD("val") ~ ConstantParser ~ T_COLON ~ T_TYPE("Double") ~ T_OP("=") ~ FloatParser) ==> { case _ ~ x ~ _ ~ _ ~ _ ~ z => FConst(x, z): Decl }

lazy val Prog: Parser[List[Token], List[Decl]] =
  (Defn ~ T_SEMI ~ Prog) ==> { case x ~ _ ~ z => x :: z : List[Decl] } ||
  (Exp ==> ((s) => List(Main(s)) : List[Decl]))

def parse_tks(tks: List[Token]) : List[Decl] = 
  Prog.parse_single(tks)

def parse(tks: List[Token]) : Set[(List[Decl], List[Token])]  = 
  Prog.parse(tks)

// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// K-language (K-expressions, K-values)
type Ty = String
type TyEnv = Map[String, String]  // [identifier, type]
type CEnv = Map[String, String]   // [constant identifier, reference identifier]

abstract class KExp
abstract class KVal
case class KVar(s: String, ty: Ty = "UNDEF") extends KVal // Note: Ty is unused, all types are stored in the environment
case class KConstVal(s: String) extends KVal
case class KNum(i: Int) extends KVal
case class KFNum(f: Float) extends KVal
case class KChConst(i: Int) extends KVal
case class Kop(o: String, v1: KVal, v2: KVal) extends KVal
case class KCall(o: String, vrs: List[KVal]) extends KVal

case object KSkip extends KVal
case object KWriteLine extends KVal
case object KWriteStar extends KVal
case object KWriteSpace extends KVal
case class KWriteInt(v: KVal) extends KVal
case class KWriteChar(v: KVal) extends KVal

case class KLet(x: String, e1: KVal, e2: KExp) extends KExp {
  override def toString = s"LET $x = $e1 in \n$e2" 
}
case class KIf(x1: String, e1: KExp, e2: KExp) extends KExp {
  def pad(e: KExp) = e.toString.replaceAll("(?m)^", "  ")

  override def toString = 
     s"IF $x1\nTHEN\n${pad(e1)}\nELSE\n${pad(e2)}"
}
case class KReturn(v: KVal) extends KExp

// translates type string from fun language to llvm-ir
def typ_convert(s: String) = s match {
  case "Int" => "i32"
  case "Double" => "double"
  case "Void" => "void"
}

// returns the type of a k-value as a string
def typ_val(v: KVal, ts: TyEnv) : String = v match {
  case KVar(s, ty) => ts(s)
  case KConstVal(s) => ts(s)
  case KNum(i) => "i32"
  case KFNum(f) => "double"
  case KChConst(i) => "i32"
  case Kop(o, v1, v2) => {
    if (typ_val(v1, ts) == "double" || typ_val(v2, ts) == "double") "double"
    else "i32"
  }
  case KCall(o, vrs) => ts(o)
  case KSkip => "void"
  case KWriteLine => "void"
  case KWriteStar => "void"
  case KWriteSpace => "void"
  case KWriteInt(e) => "void"
  case KWriteChar(e) => "void"
}

// returns the type of a k-expression as a string
def typ_exp(a: KExp, ts: TyEnv) : String = a match {
  case KLet(x, e1, e2) => typ_val(e1, ts)
  case KIf(x, e1, e2) => "il"
  case KReturn(v) => typ_val(v, ts)
}

// CPS translation from Exps to KExps using a
// continuation k.
def CPS(e: Exp, ts: TyEnv, cEnv: CEnv)(k: KVal => KExp) : KExp = e match {
  case Var(s) => k(KVar(s)) 
  case ConstVal(i) => {
    if (cEnv.contains(i)) k(KVar(cEnv(i)))
    else {
      val z = Fresh("tmp")
      KLet(z, KConstVal(i), k(KVar(z)))
    }
  }
  case Num(i) => k(KNum(i))
  case FNum(f) => k(KFNum(f))
  case ChConst(i) => k(KChConst(i))
  case Aop(o, e1, e2) => {
    val z = Fresh("tmp")
    CPS(e1, ts, cEnv)(y1 => 
      CPS(e2, ts, cEnv)(y2 => KLet(z, Kop(o, y1, y2), k(KVar(z)))))
  }
  case If(Bop(o, b1, b2), e1, e2) => {
    val z = Fresh("tmp")
    CPS(b1, ts, cEnv)(y1 => 
      CPS(b2, ts, cEnv)(y2 => 
        KLet(z, Kop(o, y1, y2), KIf(z, CPS(e1, ts, cEnv)(k), CPS(e2, ts, cEnv)(k)))))
  }
  case Call(name, args) => {
    def aux(args: List[Exp], vs: List[KVal]) : KExp = args match {
      case Nil => {
        val z = Fresh("tmp")
        KLet(z, KCall(name, vs), k(KVar(z)))
      }
      case e::es => CPS(e, ts, cEnv)(y => aux(es, vs ::: List(y)))
    }
    aux(args, Nil)
  }
  case Sequence(e1, e2) => 
    CPS(e1, ts, cEnv)(_ => CPS(e2, ts, cEnv)(y2 => k(y2)))
  case Skip => k(KSkip)
  case WriteLine => {
    val z = Fresh("tmp")
    KLet(z, KWriteLine, k(KVar(z)))
  }
  case WriteStar => {
    val z = Fresh("tmp")
    KLet(z, KWriteStar, k(KVar(z)))
  }
  case WriteSpace => {
    val z = Fresh("tmp")
    KLet(z, KWriteSpace, k(KVar(z)))
  }
  case WriteInt(e) => {
    val z = Fresh("tmp")
    CPS(e, ts, cEnv)(y => KLet(z, KWriteInt(y), k(KVar(z))))
  }
  case WriteChar(e) => {
    val z = Fresh("tmp")
    CPS(e, ts, cEnv)(y => KLet(z, KWriteChar(y), k(KVar(z))))
  }
}   

// initial continuation
def CPSi(e: Exp, ts: TyEnv, cEnv: CEnv) = CPS(e, ts, cEnv)(KReturn)

// convenient string interpolations 
// for instructions, labels and methods
implicit def sring_inters(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
    def m(args: Any*): String = sc.s(args:_*) ++ "\n"
}

// mathematical and boolean operations for integers
def compile_op(op: String) = op match {
  case "+" => "add i32 "
  case "*" => "mul i32 "
  case "-" => "sub i32 "
  case "/" => "sdiv i32 "
  case "%" => "srem i32 "
  case "==" => "icmp eq i32 "
  case "!=" => "icmp ne i32 "
  case "<=" => "icmp sle i32 "     // signed less or equal
  case "<"  => "icmp slt i32 "     // signed less than
  case ">=" => "icmp sge i32 "     // signed greater or equal
  case ">"  => "icmp sgt i32 "     // signed greater than
}

// mathematical and boolean operations for floats
def compile_dop(op: String) = op match {
  case "+" => "fadd double "
  case "*" => "fmul double "
  case "-" => "fsub double "
  case "/" => "fdiv double "
  case "%" => "frem double "
  case "==" => "fcmp oeq double "
  case "!=" => "fcmp one double "
  case "<=" => "fcmp ole double "     // signed less or equal
  case "<"  => "fcmp olt double "     // signed less than
  case ">=" => "fcmp oge double "     // signed greater or equal
  case ">"  => "fcmp ogt double "     // signed greater than
}

def compile_val(v: KVal, ts: TyEnv, cEnv: CEnv) : String = v match {
  case KNum(i) => s"$i"
  case KFNum(f) => s"$f"
  case KChConst(i) => s"$i"
  case KVar(s, ty) => s"%$s"
  case KConstVal(i) => s"%${cEnv(i)}"
  case Kop(op, x1, x2) => {
    if (typ_val(x1, ts) == "i32") s"${compile_op(op)} ${compile_val(x1, ts, cEnv)}, ${compile_val(x2, ts, cEnv)}"
    else s"${compile_dop(op)} ${compile_val(x1, ts, cEnv)}, ${compile_val(x2, ts, cEnv)}"
  }
  case KCall(x1, args) => 
    s"call ${ts(x1)} @$x1 (${args.map(x => typ_val(x, ts) + " " + compile_val(x, ts, cEnv)).mkString("",", ", "")})"
  case KSkip =>
    s"call void @skip ()"
  case KWriteLine =>
    s"call void @new_line ()"
  case KWriteStar =>
    s"call void @print_star ()"
  case KWriteSpace =>
    s"call void @print_space ()"
  case KWriteInt(x1) =>
    s"call void @print_int (i32 ${compile_val(x1, ts, cEnv)})"
  case KWriteChar(x1) =>
    s"call void @print_char (i32 ${compile_val(x1, ts, cEnv)})"
}

// load constant to a local variable if required
def check_constant(x: String, v: KVal, ts: TyEnv, cEnv: CEnv) : (String, CEnv) = v match {
  case KConstVal(i) => {
    if (cEnv.contains(i)) ("", cEnv)
    else {
      val vType = typ_val(v, ts)
      (i"%$x = load ${vType}, ${vType}* @$i", cEnv + (i -> x))
    }
  }
  case _ => ("", cEnv)
}

def compile_exp(a: KExp, ts: TyEnv, cEnv: CEnv) : (String, TyEnv, CEnv) = a match {
  case KReturn(v) => {
    if (typ_val(v, ts) == "void") v match {
      case KVar(s, ty) => (i"ret void", ts, cEnv)
      case KConstVal(s) => (i"ret void", ts, cEnv)
      case _ => (i"${compile_val(v, ts, cEnv)}" ++ i"ret void", ts, cEnv)
    }
    else (i"ret ${typ_val(v, ts)} ${compile_val(v, ts, cEnv)}", ts, cEnv)
  }
  case KLet(x: String, v: KVal, e: KExp) => {
    val newTs = ts + (x -> typ_val(v, ts))
    
    if (typ_val(v, ts) == "void") (i"${compile_val(v, ts, cEnv)}" ++ compile_exp(e, newTs, cEnv)._1, newTs, cEnv)
    else v match {
      case KConstVal(i) => {
        val (preInstr, newCEnv) = check_constant(x, v, ts, cEnv)
        (preInstr ++ compile_exp(e, newTs, newCEnv)._1, newTs, newCEnv)
      }
      case _ => (i"%$x = ${compile_val(v, ts, cEnv)}" ++ compile_exp(e, newTs, cEnv)._1, newTs, cEnv)
    }
  }
  case KIf(x, e1, e2) => {
    val if_br = Fresh("if_branch")
    val else_br = Fresh("else_branch")
    (i"br i1 %$x, label %$if_br, label %$else_br" ++
    l"\n$if_br" ++
    compile_exp(e1, ts, cEnv)._1 ++
    l"\n$else_br" ++ 
    compile_exp(e2, ts, cEnv)._1, ts + (x -> "il"), cEnv)
  }
}

def loopArgs(arg: List[(String, String)], ts: TyEnv) : TyEnv = {
  if (arg.isEmpty) ts
  else loopArgs(arg.tail, ts + (arg.head._1 -> typ_convert(arg.head._2)))
}

// compile function for declarations and main
def compile_decl(d: Decl, ts: TyEnv, cEnv: CEnv) : (String, TyEnv, CEnv) = d match {
  case Def(name, args, ty, body) => { 
    val newTs = ts + (name -> typ_convert(ty))
    (m"define ${typ_convert(ty)} @$name (${args.map(x => typ_convert(x._2) + " %" + x._1).mkString("",", ", "")}) {" ++
    compile_exp(CPSi(body, newTs, cEnv), loopArgs(args, newTs), cEnv)._1 ++
    m"}\n", newTs, cEnv)
  }
  case Const(name, v) => (m"@$name = global i32 $v \n", ts + (name -> "i32"), cEnv)
  case FConst(name, x) => (m"@$name = global double $x \n", ts + (name -> "double"), cEnv)
  case Main(body) => {
    (m"define i32 @main() {" ++
    compile_exp(CPS(body, ts, cEnv)(_ => KReturn(KNum(0))), ts, cEnv)._1 ++
    m"}\n", ts, cEnv)
  }
}

// removed the new line for print_int since it didn't really make sense
val prelude = """
declare i32 @printf(i8*, ...)

@.str_nl = private constant [2 x i8] c"\0A\00"
@.str_star = private constant [2 x i8] c"*\00"
@.str_space = private constant [2 x i8] c" \00"

define void @new_line() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_star() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_space() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @skip() #0 {
  ret void
}

@.str = private constant [3 x i8] c"%d\00"

define void @print_int(i32 %x) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.str, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
   ret void
}

@.char = private constant [3 x i8] c"%c\00"

define void @print_char(i32 %x) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.char, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}

; END OF BUILD-IN FUNCTIONS (prelude)

"""

// compilation of a block (i.e. list of instructions)
def compile_block(prog: List[Decl], ts: TyEnv, cEnv: CEnv) : (String, TyEnv, CEnv) = prog match {
  case Nil => ("", ts, cEnv)
  case s::bl => {
    val (instrs1, ts1, cE1) = compile_decl(s, ts, cEnv)
    val (instrs2, ts2, cE2) = compile_block(bl, ts1, cE1)
    (instrs1 ++ instrs2, ts2, cE2)
  }
}

// main compiler functions
def compile(prog: List[Decl]) : String = {
  val instructions = compile_block(prog, Map.empty, Map.empty)._1
  prelude ++ instructions
}

@main
def write(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    val tks = tokenise(os.read(path))
    val ast = parse_tks(tks)
    val code = compile(ast)
    os.write.over(os.pwd / (file ++ ".ll"), code)
}

@main
def run(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    write(fname)  
    os.proc("llc", "-filetype=obj", file ++ ".ll").call()
    os.proc("gcc", file ++ ".o", "-o", file ++ ".bin").call()
    os.proc(os.pwd / (file ++ ".bin")).call(stdout = os.Inherit)
    println(s"done.")
}
