import granolap._

class ParserTests extends org.scalatest.FunSuite {
  test("Import statement") {
    val code = "import x as y"
    assert(GranolaParser(Lexer(code)) === Assertion(List(ImportStatement(Identifier("x"),Identifier("y"))),List()))
  }

  test("Multi-statement header") {
    val code = "import x as y\ntypedef CoolInt as Int\nimport matches as _matches"
    assert(GranolaParser(Lexer(code)) === Assertion(List(ImportStatement(Identifier("x"),Identifier("y")), TypedefStatement(Identifier("CoolInt"),IntType), ImportStatement(Identifier("matches"),Identifier("_matches"))),List()))
  }

  test("Simple assertion") {
    val code = "assert x"
    assert(GranolaParser(Lexer(code)) === Assertion(List(),List(FunctionCall(Identifier("assert"),List(VariableExpression(Identifier("x")))))))
  }

  test("Defined function") {
    val code =
      """
        def foo(y: Bool) -> Bool {
           not y
        }

        assert foo z
      """.stripMargin
    assert(GranolaParser(Lexer(code)) === Assertion(List(),List(FunctionDef(Identifier("foo"),List(Param(Identifier("y"),BoolType)),BoolType,FunctionCall(Identifier("not"),List(VariableExpression(Identifier("y"))))), FunctionCall(Identifier("assert"),List(FunctionCall(Identifier("foo"),List(VariableExpression(Identifier("z")))))))))
  }

  test("Function call") {
    val code = "function(a, not b, not not c)"
    assert(GranolaParser(Lexer(code)) === Assertion(List(),List(FunctionCall(Identifier("function"),List(VariableExpression(Identifier("a")), FunctionCall(Identifier("not"),List(VariableExpression(Identifier("b")))), FunctionCall(Identifier("not"),List(FunctionCall(Identifier("not"),List(VariableExpression(Identifier("c")))))))))))
  }

  test("If expression") {
    val code = "if x { y } else { z }"
    assert(GranolaParser(Lexer(code)) === Assertion(List(),List(IfExpression(List(IfSubExpression(VariableExpression(Identifier("x")),VariableExpression(Identifier("y")))),VariableExpression(Identifier("z"))))))
  }

  test("If/else if expression") {
    val code = "if x { a } else if dec x { b } else if y { b } else { c }"
    assert(GranolaParser(Lexer(code)) === Assertion(List(),List(IfExpression(List(IfSubExpression(VariableExpression(Identifier("x")),VariableExpression(Identifier("a"))), IfSubExpression(FunctionCall(Identifier("dec"),List(VariableExpression(Identifier("x")))),VariableExpression(Identifier("b"))), IfSubExpression(VariableExpression(Identifier("y")),VariableExpression(Identifier("b")))),VariableExpression(Identifier("c"))))))
  }

  test("Let expression") {
    val code = "let x = 1 in decrement x"
    assert(GranolaParser(Lexer(code)) === Assertion(List(),List(LetExpression(Identifier("x"),IntConstantExpr(IntConstant("1")),FunctionCall(Identifier("decrement"),List(VariableExpression(Identifier("x"))))))))
  }

}