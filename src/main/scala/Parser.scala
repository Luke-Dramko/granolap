package granolap

import scala.util.parsing.combinator._

object GranolaParser extends Parsers {
  override type Elem = Token

  private def identifier: Parser[Identifier] = accept("identifier", {case id @ Identifier(_) => id})

  private def floatconst: Parser[FloatConstant] = accept("float constant", {case fc @ FloatConstant(_) => fc})

  private def boolconst: Parser[BoolConstant] = accept("bool constant", {case bc @ BoolConstant(_) => bc})

  private def intconst: Parser[IntConstant] = accept("int constant", {case ic @ IntConstant(_) => ic})

  private def stringconst: Parser[StringConstant] = accept("string constant", {case sc @ StringConstant(_) => sc})

  def program: Parser[Program] = {
    //phrase(rep(statement) ~ rep1(expression))
    phrase(rep(statement) ~ rep(expression)) ^^ { case stmtlist ~ exprlist => Assertion(stmtlist, exprlist)}
  }

  def statement: Parser[Statement] = {
    val importStatement = Import ~ identifier ~ As ~ identifier ^^ { case _ ~ name ~ _ ~ rename => ImportStatement(name, rename)}
    val typedefStatement = Typedef ~ identifier ~ As ~ _type ^^ { case _ ~ t ~ _ ~ ascripted => TypedefStatement(t, ascripted)}

    importStatement | typedefStatement
  }

  //*** Handle more complex types as well!!!!
  def _type: Parser[Type] = {
    _Int ^^ { _ => IntType } | _UInt ^^ { _ => UIntType } | _Float ^^ { _ => FloatType } | _Bool ^^ { _ => BoolType } |
    _String ^^ { _ => StringType }
  }

  def definedFunction: Parser[DefinedFunction] = {
    val argexpr = {identifier ~ Colon ~ _type ~ rep(Comma ~ identifier ~ Colon ~ _type)}.?

    println(argexpr)

    Def ~ identifier ~ LParen ~ {identifier ~ Colon ~ _type ~ rep(Comma ~ identifier ~ Colon ~ _type)}.? ~ RParen ~
    Arrow ~ LCurlyBrace ~ expression ~ RCurlyBrace ^^
      { case _ ~ name ~ _ ~ args ~ _ ~ _ ~ _ ~ body ~ _ => null } //*** placeholder
  }

  def expression: Parser[Expression] = {

    //*** Missing support for else ifs.
    val ifexpr = If ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace ~ rep(Else ~ If ~ expression ~ LCurlyBrace ~ expression ~
      RCurlyBrace) ~ Else ~ LCurlyBrace ~ expression ~ RCurlyBrace ^^
        { case _ ~ condition ~ _ ~ truebody ~ _ ~ _ ~ _ ~ _ ~ falsebody ~ _ => IfExpression(List((condition, truebody)), falsebody)}

    //*** Missing support for else if lets
    val ifletexpr = If ~ Let ~ identifier ~ EqualsSign ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace ~
      rep(Else ~ If ~ Let ~ identifier ~ EqualsSign ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace) ~
      Else ~ LCurlyBrace ~ expression ~ RCurlyBrace ^^
        { case _ ~ _ ~ id1 ~ _ ~ e1 ~ _ ~ b1 ~ _ ~ _ ~ _ ~ _ ~ bf ~_ => IfLetExpression(List((id1, e1, b1)), bf)}

    //*** Missing support for function arguments
    val fcall = identifier ~ LParen ~ { expression ~ rep(Comma ~ expression) }.? ~ RParen ^^
      { case name ~ _ ~ _ ~ _ => FunctionCall(name, List()) }

    //val arg2fcall = expression ~ identifier ~ expression ^^ { case e1 ~ name ~ e2 => FunctionCall(name, List(e1, e2)) }

    val arg1fcall = identifier ~ expression ^^ { case name ~ e => FunctionCall(name, List(e)) }

    //val anonymousfunc = identifier ~ Colon ~ _type ~ rep(Comma ~ identifier ~ Colon ~ _type) ~ Arrow ~ expression

    val letexpr = Let ~ identifier ~ EqualsSign ~ expression ~ { In.? } ~ expression ^^
      { case _ ~ variable ~ _ ~ e1 ~ _ ~ e2 => LetExpression(variable, e1, e2) }

    val parentheticalexpr = LParen ~ expression ~ RParen ^^ { case _ ~ e ~ _ => ParentheticalExpression(e)}

    val idexpr = identifier ^^ { id => VariableExpression(id) }

    //*** Missing support for tuple selection

    val boolc = boolconst ^^ { case bc => BoolConstantExpr(bc) }
    val stringc = stringconst ^^ { case sc => StringConstantExpr(sc) }
    val intc = intconst ^^ { case ic => IntConstantExpr(ic) }
    val floatc = floatconst ^^ { case fc => FloatConstantExpr(fc) }


    ifexpr | ifletexpr | fcall | arg1fcall | letexpr | parentheticalexpr | idexpr | boolc | stringc | intc | floatc
  }

  def apply(tokens: List[Token]): Program = {
    program(new TokenReader(tokens)) match {
      case NoSuccess(msg, _) => throw new Exception(msg)
      case Success(result, _) => result
    }
  }
}