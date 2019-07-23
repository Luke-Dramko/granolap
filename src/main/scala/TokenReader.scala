package granolap

import scala.util.parsing.input._

/*
A helper class to Parser which defines how tokens are read from the output list of the Lexer.
 */

class TokenReader(tokens: List[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}