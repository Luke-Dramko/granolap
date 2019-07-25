import granolap._

class LexerTests extends org.scalatest.FunSuite {
  test("Simple Assertion: x > 0") {
    assert(Lexer("assert x > 0") === List(Identifier("assert"), Identifier("x"), Identifier(">"), IntConstant("0")))
  }

  test("Multiline Assertion") {
    assert(Lexer("import x as y\n\nassert if y == 7.4 { true } else { false }") ===
      List(Import, Identifier("x"), As, Identifier("y"), Identifier("assert"), If, Identifier("y"),
        Identifier("=="), FloatConstant("7.4"), LCurlyBrace, BoolConstant("true"), RCurlyBrace, Else, LCurlyBrace, BoolConstant("false"), RCurlyBrace))
  }

  test("Function Assertion") {
    assert(Lexer("def foo(x: Int, y: Float, z: Bool) -> String { \n string(x) + y + z } \n\n assert stuff == foo(a, b, c)") ===
    List(Def, Identifier("foo"), LParen, Identifier("x"), Colon, _Int, Comma, Identifier("y"), Colon, _Float, Comma, Identifier("z"), Colon, _Bool,
      RParen, Arrow, _String, LCurlyBrace, Identifier("string"), LParen, Identifier("x"), RParen, Identifier("+"), Identifier("y"),
      Identifier("+"), Identifier("z"), RCurlyBrace, Identifier("assert"), Identifier("stuff"), Identifier("=="),
      Identifier("foo"), LParen, Identifier("a"), Comma, Identifier("b"), Comma, Identifier("c"), RParen))
  }

  test("All Tokens") {
    assert(Lexer("bacon 1 1.0 \"stuff\" true if else let case default import typedef as def Int UInt Float String Bool Enum" +
      " enum Null null -> = (){}[],in .?") ===
      List(Identifier("bacon"), IntConstant("1"), FloatConstant("1.0"), StringConstant("stuff"), BoolConstant("true"), If, Else,
        Let, Case, Default, Import, Typedef, As, Def, _Int, _UInt, _Float, _String, _Bool, _Enum, EnumValue, _Null, NullValue,
        Arrow, EqualsSign, LParen, RParen, LCurlyBrace, RCurlyBrace, LBracket, RBracket, Comma, In, Dot, QuestionMark))
  }
}