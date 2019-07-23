package granolap

import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

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
    """"([^"]|\")*"""".r ^^ { str => StringConstant(str.substring(1, str.length - 1)) }
  }

  private[this] def boolconstant: Parser[BoolConstant] = {
    "(true)|(false)".r ^^ { str => BoolConstant(str) }
  }

  private[this] def _if: Parser[Token] = {
	  "if" ^^ { str => If}
  }

  private[this] def _else: Parser[Token] = {
	  "else" ^^ { str => Else }
  }

  private[this] def _let: Parser[Token] = {
	  "let" ^^ { str => Let }
  }

  private[this] def _case: Parser[Token] = {
	  "case" ^^ { str => Case }
  }

  private[this] def _default: Parser[Token] = {
	  "default" ^^ { str => Default }
  }

  private[this] def _import: Parser[Token] = {
	  "import" ^^ { str => Import }
  }

  private[this] def _typedef: Parser[Token] = {
	  "typedef" ^^ { str => Typedef }
  }

  private[this] def _as: Parser[Token] = {
	  "as" ^^ { str => As }
  }

  private[this] def _def: Parser[Token] = {
	  "def" ^^ { str => Def }
  }

  private[this] def _int: Parser[Token] = {
	  "Int" ^^ { str => _Int }
  }

  private[this] def _uint: Parser[Token] = {
	  "UInt" ^^ { str => _UInt }
  }

  private[this] def _float: Parser[Token] = {
	  "Float" ^^ { str => _Float }
  }

  private[this] def _string: Parser[Token] = {
	  "String" ^^ { str => _String }
  }

  private[this] def _bool: Parser[Token] = {
	  "Bool" ^^ { str => _Bool }
  }

  private[this] def _enum: Parser[Token] = {
	  "Enum" ^^ { str => _Enum }
  }

  private[this] def enumValue: Parser[Token] = {
	  "enum" ^^ { str => EnumValue }
  }

  private[this] def _null: Parser[Token] = {
	  "Null" ^^ { str => _Null }
  }

  private[this] def nullValue: Parser[Token] = {
	  "null" ^^ { str => NullValue }
  }

  private[this] def arrow: Parser[Token] = {
	  "->" ^^ { str => Arrow }
  }

  private[this] def equalsSign: Parser[Token] = {
	  "=" ^^ { str => EqualsSign }
  }

  private[this] def newLine: Parser[Token] = {
	  "\n" ^^ { str => NewLine }
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

  private[this] def in: Parser[Token] = {
    "in" ^^ { str => In }
  }

  /**
    * Tokenizes the program string.
    *
    * @return a list of tokens representing the program string.
    */
  def tokens: Parser[List[Token]] = {
    phrase(rep1(_if | _else | _let | _case | _default | _import | _typedef |  _def | _int | _uint | _float | _string
    | _bool | _enum | enumValue | _null | nullValue | arrow | newLine | in | boolconstant | identifier |
      floatconstant | intconstant | stringconstant | lParen | rParen | lCurlyBrace | rCurlyBrace |
    lBracket | rBracket | comma )) ^^ { raw => postprocessing(raw) }
  }

  /**
    * Modifies the token stream.
    * Modificiations include
    * - All instances of "as" as an identifier are replaced with the "As" token
    * - All instances of "=" as an identifier are replaced with the "EqualsSign" token
    *
    * @param ts a list of tokens
    * @return Modified list of tokens
    */
  def postprocessing(ts: List[Token]): List[Token] = ts match {
    case Nil => Nil
    case t :: ts => t match {
      case Identifier(id) => if (id == "as") As :: postprocessing(ts)
                             else if (id == "=") EqualsSign :: postprocessing(ts)
                             else Identifier(id) :: postprocessing(ts)
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
