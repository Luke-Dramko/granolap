package granolap

import scala.util.parsing.combinator._

object GranolaParser extends Parsers {
//  private var lr2argfunc: Boolean = true
//  private var lrtuple: Boolean = true
//  private var lroptional: Boolean = true
//  private var lrascription: Boolean = true

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

  def params: Parser[List[Param]] = {
    val more = identifier ~ Colon ~ _type ~ Comma ~ params ^^ { case name ~ _ ~ t ~ _ ~ p => p ++ List(Param(name, t)) }
    val last = identifier ~ Colon ~ _type ~ RParen ^^ { case name ~ _ ~ t ~ _ => List(Param(name, t))}
    val none = RParen ^^ { case _ => List() }

    none | last | more
  }

  def definedFunction: Parser[Expression] = {
    //RParen is explicitly left out as it is handled by the params function.
    Def ~ identifier ~ LParen ~ params ~ Arrow ~ _type ~ LCurlyBrace ~ expression ~ RCurlyBrace ^^
      { case _ ~ name ~ _ ~ ps ~ _ ~ t ~ _ ~ body ~ _ => FunctionDef(name, ps, t, body) }
  }

  def args: Parser[List[Expression]] = {
    val more = expression ~ Comma ~ args ^^ { case e ~ _ ~ a => List(e) ++ a }
    val last = expression ~ RParen ^^ { case e ~ _ => List(e) }
    val none = RParen ^^ { case _ => List() }

    none | last | more
  }

  def elseifs: Parser[List[IfSubExpression]] = {
    val more = Else ~ If ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace ~ elseifs ^^
      { case _ ~ _ ~ condition ~ _ ~ body ~ _ ~ additional => List(IfSubExpression(condition, body)) ++ additional }
    val none = Else ^^ { case _ => List[IfSubExpression]() }

    more | none
  }

  def elseiflets: Parser[List[IfLetSubExpression]] = {
    val more = Else ~ If ~ Let ~ identifier ~ EqualsSign ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace ~ elseiflets ^^
      { case _ ~ _ ~ _ ~ id ~ _ ~ condition ~ _ ~ body ~ _ ~ additional => List(IfLetSubExpression(id, condition, body)) ++ additional }
    val none = Else ^^ { case _ => List[IfLetSubExpression]() }

    more | none
  }

  def expression: Parser[Expression] = {

    //The else is consumed by the elseifs function as a delimiter.
    val ifexpr = If ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace ~ elseifs ~ LCurlyBrace ~ expression ~ RCurlyBrace ^^
        { case _ ~ condition ~ _ ~ truebody ~ _ ~ eifs ~ _ ~ falsebody ~ _ =>
          IfExpression(IfSubExpression(condition, truebody) :: eifs, falsebody)}

    //*** Missing support for else if lets
    val ifletexpr = If ~ Let ~ identifier ~ EqualsSign ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace ~
      elseiflets ~ LCurlyBrace ~ expression ~ RCurlyBrace ^^
        { case _ ~ _ ~ id1 ~ _ ~ e1 ~ _ ~ b1 ~ _ ~ eifls ~ _ ~ bf ~ _ =>
          IfLetExpression(IfLetSubExpression(id1, e1, b1) :: eifls, bf)}

    val caseexpr = Case ~ identifier ~ RCurlyBrace ~ rep(casepattern ~ Arrow ~ expression) ~
      { Default ~ Arrow ~ expression }.? ~ LCurlyBrace ^^
      { case _ ~ variable ~ _ ~ _ ~ _ ~ _ => CaseExpression(variable, List()) }

    //Right parenthesis is purposefully missing as it is consumed by the args function as a delimiter.
    val fcall = identifier ~ LParen ~ args ^^ { case name ~ _ ~ ps => FunctionCall(name, ps) }

    //val arg2fcall = expression ~ identifier ~ expression ^^ { case e1 ~ name ~ e2 => FunctionCall(name, List(e1, e2)) }

    val arg1fcall = identifier ~ expression ^^ { case name ~ e => FunctionCall(name, List(e)) }

    //val anonymousfunc = identifier ~ Colon ~ _type ~ rep(Comma ~ identifier ~ Colon ~ _type) ~ Arrow ~ expression

    //*** Missing support for cases.
    val letexpr = Let ~ identifier ~ EqualsSign ~ expression ~ In.? ~ expression ^^
      { case _ ~ variable ~ _ ~ e1 ~ _ ~ e2 => LetExpression(variable, e1, e2) }

    val parentheticalexpr = LParen ~ expression ~ RParen ^^ { case _ ~ e ~ _ => ParentheticalExpression(e)}

    val idexpr = identifier ^^ { id => VariableExpression(id) }

    val definedfuncexpr = definedFunction

    //*** Missing support for type ascription and optionalization (left recursive)
    //*** Missing support for tuple selection (left recursive)

    val boolc = boolconst ^^ { case bc => BoolConstantExpr(bc) }
    val stringc = stringconst ^^ { case sc => StringConstantExpr(sc) }
    val intc = intconst ^^ { case ic => IntConstantExpr(ic) }
    val floatc = floatconst ^^ { case fc => FloatConstantExpr(fc) }
    val nullc = NullValue ^^ { case _ => NullExpression }


    ifexpr | ifletexpr | fcall | arg1fcall | letexpr | parentheticalexpr | definedfuncexpr |
      idexpr | boolc | stringc | intc | floatc | nullc
  }

  def casepattern: Parser[CasePattern] = {
    val typecp = Let ~ identifier ~ Colon ~ _type ^^ { case _ ~ x ~ _ ~ t => LetCasePattern(x, t)}
    val enumcp = rep(identifier) ^^ { case choices => ListCasePattern(choices) }

    typecp | enumcp
  }

  def apply(tokens: List[Token]): Program = {
    program(new TokenReader(tokens)) match {
      case NoSuccess(msg, _) => throw new ParsingException(msg)
      case Success(result, _) => result
    }
  }
}