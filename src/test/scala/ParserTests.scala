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
        def foo(y: Bool, z: Int) -> Bool {
           not y
        }

        assert foo z
      """.stripMargin
    assert(GranolaParser(Lexer(code)) === Assertion(List(),List(FunctionDef(Identifier("foo"),List(Param(Identifier("y"),BoolType), Param(Identifier("z"),IntType)),BoolType,FunctionCall(Identifier("not"),List(VariableExpression(Identifier("y"))))), FunctionCall(Identifier("assert"),List(FunctionCall(Identifier("foo"),List(VariableExpression(Identifier("z")))))))))
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

  test("Case expression for enums") {
    val code =
      """
        case x {
           Monday -> 2
           Friday, Saturday, Sunday -> 3
           default -> 1
        }
      """.stripMargin
    assert(GranolaParser(Lexer(code)) === Assertion(List(),List(CaseExpression(Identifier("x"),List(CaseEntry(ListCasePattern(List(Identifier("Monday"))),IntConstantExpr(IntConstant("2"))), CaseEntry(ListCasePattern(List(Identifier("Friday"), Identifier("Saturday"), Identifier("Sunday"))),IntConstantExpr(IntConstant("3"))), CaseEntry(DefaultCasePattern,IntConstantExpr(IntConstant("1"))))))))
  }

  test("Case expression for general sum type") {
    val code =
      """
        case x {
           let x: Int -> x
           let y: Float -> toint y
           default -> 0
        }
      """.stripMargin

    assert(GranolaParser(Lexer(code)) === Assertion(List(),List(CaseExpression(Identifier("x"),List(CaseEntry(LetCasePattern(Identifier("x"),IntType),VariableExpression(Identifier("x"))), CaseEntry(LetCasePattern(Identifier("y"),FloatType),FunctionCall(Identifier("toint"),List(VariableExpression(Identifier("y"))))), CaseEntry(DefaultCasePattern,IntConstantExpr(IntConstant("0"))))))))
  }

  test("Unlabeled tuple") {
    val code =
      """
        typedef node as (Node, Array[Int])
      """.stripMargin

    assert(GranolaParser(Lexer(code)) === Assertion(List(TypedefStatement(Identifier("node"),TupleType(List(LabeledType(IndexLabel(1),DefinedType(Identifier("Node"))), LabeledType(IndexLabel(2),ArrayType(IntType)))))),List()))
  }

  test("Labeled tuple") {
    val code =
      """
        typedef node as (next: Node, elements: Array[Int])
      """.stripMargin

    assert(GranolaParser(Lexer(code)) === Assertion(List(TypedefStatement(Identifier("node"),TupleType(List(LabeledType(IdentifierLabel(Identifier("next")),DefinedType(Identifier("Node"))), LabeledType(IdentifierLabel(Identifier("elements")),ArrayType(IntType)))))),List()))
  }

  test("Sum type") {
    val code =
      """
        typedef randomsumtype as (Int or Float or (Float) -> Int)
      """.stripMargin

    assert(GranolaParser(Lexer(code)) === Assertion(List(TypedefStatement(Identifier("randomsumtype"),SumType(List(IntType, FloatType, FunctionType(List(FloatType),IntType))))),List()))
  }

  test("Enum type") {
    val code = "typedef weekdays as Enum(Monday, Tuesday, Wednesday, Thursday, Friday)"

    assert(GranolaParser(Lexer(code)) === Assertion(List(TypedefStatement(Identifier("weekdays"),EnumType(List(Identifier("Monday"), Identifier("Tuesday"), Identifier("Wednesday"), Identifier("Thursday"), Identifier("Friday"))))),List()))
  }

  test("Function type") {
    val code = "typedef Func as (Int, Enum(SMALL, MEDIUM, LARGE), (Int) -> Int, (Bool or Error)) -> Int"

    assert(GranolaParser(Lexer(code)) === Assertion(List(TypedefStatement(Identifier("Func"),FunctionType(List(IntType, EnumType(List(Identifier("SMALL"), Identifier("MEDIUM"), Identifier("LARGE"))), FunctionType(List(IntType),IntType), SumType(List(BoolType, DefinedType(Identifier("Error"))))),IntType))),List()))
  }
}