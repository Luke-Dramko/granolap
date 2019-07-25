package granolap

import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f\n]+".r

  private[this] def identifier: Parser[Identifier] = {
    """[-a-zA-Z!%&|!<>*_=+][-a-zA-Z0-9!%&|!<>*_=+]*""".r ^^ { str => Identifier(str) }
  }

  private[this] def intconstant: Parser[IntConstant] = {
    "[0-9]+".r ^^ { str => IntConstant(str) }
  }

  private[this] def floatconstant: Parser[FloatConstant] = {
    """[0-9]+\.([0-9]*)?""".r ^^ { str => FloatConstant(str) }
  }

  private[this] def stringconstant: Parser[StringConstant] = {
    """"([^"\\]|\\")*"""".r ^^ { str => StringConstant(str.substring(1, str.length - 1)) }
  }

  private[this] def boolconstant: Parser[BoolConstant] = {
    "(true)|(false)".r ^^ { str => BoolConstant(str) }
  }

  private[this] def lParen: Parser[Token] = {
    "(" ^^ { str => LParen }
  }

  private[this] def rParen: Parser[Token] = {
    ")" ^^ { str => RParen }
  }

  private[this] def lCurlyBrace: Parser[Token] = {
    "{" ^^ { str => LCurlyBrace }
  }

  private[this] def rCurlyBrace: Parser[Token] = {
    "}" ^^ { str => RCurlyBrace }
  }

  private[this] def lBracket: Parser[Token] = {
    "[" ^^ { str => LBracket }
  }

  private[this] def rBracket: Parser[Token] = {
    "]" ^^ { str => RBracket }
  }

  private[this] def comma: Parser[Token] = {
    "," ^^ { str => Comma }
  }

  private[this] def colon: Parser[Token] = {
    ":" ^^ { str => Colon }
  }

  private[this] def dot: Parser[Token] = {
    "." ^^ { str => Dot }
  }

  private[this] def questionMark: Parser[Token] = {
    "?" ^^ { str => QuestionMark }
  }

  /**
    * Tokenizes the program string.
    *
    * @return a list of tokens representing the program string.
    */
  def tokens: Parser[List[Token]] = {
    phrase(rep1( boolconstant | identifier | floatconstant | intconstant | stringconstant | lParen | rParen |
      lCurlyBrace | rCurlyBrace | lBracket | rBracket | comma | colon | dot | questionMark )) ^^
      { raw => postprocessing(raw) }
  }

  /**
    * Modifies the token stream.
    * Modificiations include
    * - All keyword are changed from Identifiers to the appropriate Token type. This includes = and ->, which would be a valid identifier were they not keywords.
    *
    * @param ts a list of tokens
    * @return Modified list of tokens
    */
  def postprocessing(ts: List[Token]): List[Token] = ts match {
    case Nil => Nil
    case t :: ts => t match {
      case Identifier(id) => {
        if (id == "if") If :: postprocessing(ts)
        else if (id == "else") Else :: postprocessing(ts)
        else if (id == "let") Let :: postprocessing(ts)
        else if (id == "def") Def :: postprocessing(ts)
        else if (id == "case") Case :: postprocessing(ts)
        else if (id == "import") Import :: postprocessing(ts)
        else if (id == "typedef") Typedef :: postprocessing(ts)
        else if (id == "=") EqualsSign :: postprocessing(ts)
        else if (id == "->") Arrow :: postprocessing(ts)
        else if (id == "as") As :: postprocessing(ts)
        else if (id == "default") Default :: postprocessing(ts)
        else if (id == "in") In :: postprocessing(ts)
        else if (id == "Int") _Int :: postprocessing(ts)
        else if (id == "UInt") _UInt :: postprocessing(ts)
        else if (id == "String") _String :: postprocessing(ts)
        else if (id == "Float") _Float :: postprocessing(ts)
        else if (id == "Bool") _Bool :: postprocessing(ts)
        else if (id == "Array") _Array :: postprocessing(ts)
        else if (id == "Enum") _Enum :: postprocessing(ts)
        else if (id == "enum") EnumValue :: postprocessing(ts)
        else if (id == "Null") _Null :: postprocessing(ts)
        else if (id == "null") NullValue :: postprocessing(ts)
        else if (id == "or") Or :: postprocessing(ts)
        else Identifier(id) :: postprocessing(ts)
      }
      case x: Token => x :: postprocessing(ts)
    }
  }

  /**
    *
    *
    * @param code A string representing a granola specification
    * @return A list of tokens formed from the string
    * @throws LexicalException(message) if the string cannot be tokenized
    */
  def apply(code: String): List[Token] = {
    parse(tokens, code) match {
      case NoSuccess(msg, _) => throw new LexicalException(msg)
      case Success(result, _) => result
    }
  }
}
