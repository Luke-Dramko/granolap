package granolap

import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def identifier: Parser[Identifier] = {
    """[-a-zA-Z!%&|!<>*_=+][-a-zA-Z0-9!%&|!<>*_=+]*""".r ^^ { str => Identifier(str) }
  }

  def intconstant: Parser[IntConstant] = {
    "[0-9]+".r ^^ { str => IntConstant(str) }
  }

  def floatconstant: Parser[FloatConstant] = {
    """[0-9]+\.([0-9]*)?""".r ^^ { str => FloatConstant(str) }
  }

  def stringconstant: Parser[StringConstant] = {
    """"([^"]|\")*"""".r ^^ { str => StringConstant(str.substring(1, str.length - 1)) }
  }

  def boolconstant: Parser[BoolConstant] = {
    "(true)|(false)".r ^^ { str => BoolConstant(str) }
  }
  
  def _if: Parser[Token] = {
	  "if" ^^ { str => If}
  }
  
  def _else: Parser[Token] = {
	  "else" ^^ { str => Else }
  }
  
  def _let: Parser[Token] = {
	  "let" ^^ { str => Let }
  }
  
  def _case: Parser[Token] = {
	  "case" ^^ { str => Case }
  }
  
  def _default: Parser[Token] = {
	  "default" ^^ { str => Default }
  }
  
  def _import: Parser[Token] = {
	  "import" ^^ { str => Import }
  }
  
  def _typedef: Parser[Token] = {
	  "typedef" ^^ { str => Typedef }
  }
  
  def _as: Parser[Token] = {
	  "as" ^^ { str => As }
  }
  
  def _def: Parser[Token] = {
	  "def" ^^ { str => Def }
  }
  
  def _int: Parser[Token] = {
	  "Int" ^^ { str => _Int }
  }
  
  def _uint: Parser[Token] = {
	  "UInt" ^^ { str => _UInt }
  }
  
  def _float: Parser[Token] = {
	  "Float" ^^ { str => _Float }
  }
  
  def _string: Parser[Token] = {
	  "String" ^^ { str => _String }
  }
  
  def _bool: Parser[Token] = {
	  "Bool" ^^ { str => _Bool }
  }
  
  def _enum: Parser[Token] = {
	  "Enum" ^^ { str => _Enum }
  }
  
  def enumValue: Parser[Token] = {
	  "enum" ^^ { str => EnumValue }
  }
  
  def _null: Parser[Token] = {
	  "Null" ^^ { str => _Null }
  }
  
  def nullValue: Parser[Token] = {
	  "null" ^^ { str => NullValue }
  }
  
  def arrow: Parser[Token] = {
	  "->" ^^ { str => Arrow }
  }
  
  def equalsSign: Parser[Token] = {
	  "=" ^^ { str => EqualsSign }
  }
  
  def newLine: Parser[Token] = {
	  "\n" ^^ { str => NewLine }
  }

  def lParen: Parser[Token] = {
    "(" ^^ { str => LParen }
  }

  def rParen: Parser[Token] = {
    ")" ^^ { str => RParen }
  }

  def lCurlyBrace: Parser[Token] = {
    "{" ^^ { str => LCurlyBrace }
  }

  def rCurlyBrace: Parser[Token] = {
    "}" ^^ { str => RCurlyBrace }
  }

  def lBracket: Parser[Token] = {
    "[" ^^ { str => LBracket }
  }

  def rBracket: Parser[Token] = {
    "]" ^^ { str => RBracket }
  }

  def comma: Parser[Token] = {
    "," ^^ { str => Comma }
  }

  /**
    * It's only necessary to call this method in order to lexically analyze an entire program string.
    */
  def tokens: Parser[List[Token]] = {
    phrase(rep1(_if | _else | _let | _case | _default | _import | _typedef |  _def | _int | _uint | _float | _string
    | _bool | _enum | enumValue | _null | nullValue | arrow | newLine | boolconstant | identifier |
      floatconstant | intconstant | stringconstant | lParen | rParen | lCurlyBrace | rCurlyBrace |
    lBracket | rBracket | comma )) ^^ { raw => postprocessing(raw) }
  }

  /**
    * Modifies the token stream.
    * Modificiations include
    * - All instances of "as" as an identifier are replaced with the "As" token
    *
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

  def apply(code: String): List[Token] = {
    parse(tokens, code) match {
      case NoSuccess(msg, _) => throw new Exception(msg)
      case Success(result, _) => result
    }
  }
}
