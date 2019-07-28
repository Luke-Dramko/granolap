package granolap

import scala.util.parsing.combinator._

object GranolaParser extends Parsers {
  override type Elem = Token

  //These methods convert a Token case class into the parser of the appropriate type.
  private def identifier: Parser[Identifier] = accept("identifier", {case id @ Identifier(_) => id})

  private def floatconst: Parser[FloatConstant] = accept("float constant", {case fc @ FloatConstant(_) => fc})

  private def boolconst: Parser[BoolConstant] = accept("bool constant", {case bc @ BoolConstant(_) => bc})

  private def intconst: Parser[IntConstant] = accept("int constant", {case ic @ IntConstant(_) => ic})

  private def stringconst: Parser[StringConstant] = accept("string constant", {case sc @ StringConstant(_) => sc})

  def assertion: Parser[Assertion] = {
    phrase(rep(statement) ~ rep(expression)) ^^ { case stmtlist ~ exprlist => Assertion(stmtlist, exprlist)}
  }

  /**
    * There are two types of statements: import statements, which allow you to rename a variable if it's named in a schema
    * file, and typedef statements, which allow you to rename a type.
    * @return A parser for statements.
    */
  def statement: Parser[Statement] = {
    val importStatement = Import ~ identifier ~ As ~ identifier ^^ { case _ ~ name ~ _ ~ rename => ImportStatement(name, rename)}
    val typedefStatement = Typedef ~ identifier ~ As ~ _type ^^ { case _ ~ t ~ _ ~ ascripted => TypedefStatement(t, ascripted)}

    importStatement | typedefStatement
  }

  //Helper methods for the _type method

  //Note that tuples require two or more different elements. It is not possible to declare an empty tuple or one with
  //one element. () and (Int) are not allowed. (Int, String) is.

  /**
    * The tuplets method is a helper method which helps to build a list of element types for tuples with unlabeled
    * elements. Because the tuple's elements are unlabeled, we use default labels: integers. The integer labels are
    * incremeted by one for each element of the tuple.
    *
    * This method helps to enforce that a tuple is made of two or more elements.
    *
    * @param index The label for the integer.
    * @return a parser of a list of labeled elements.
    */
  def tuplets(index: Int): Parser[List[LabeledElement]] = {
    val one = Comma ~ _type ~ RParen ^^ { case _ ~ t ~ _ => List(LabeledElement(IndexLabel(index), t)) }
    val more = Comma ~ _type ~ tuplets(index + 1) ^^ { case _ ~ t ~ ts => List(LabeledElement(IndexLabel(index), t)) ++ ts }

    one | more
  }

  /**
    * The labeledtuplets method is a helper method which helps to build a list of element types for tuples with labeled
    * elements.
    *
    * This method helps to enforce that a tuple is made of two or more elements.
    *
    * @return a parser of a list of labeled elements.
    */
  def labeledtuplets: Parser[List[LabeledElement]] = {
    val one = Comma ~ identifier ~ Colon ~ _type ~ RParen ^^ { case _ ~ l ~ _ ~ t ~ _ => List(LabeledElement(IdentifierLabel(l), t))}
    val more = Comma ~ identifier ~ Colon ~ _type ~ labeledtuplets ^^
      { case _ ~ l ~ _ ~ t ~ ts => List(LabeledElement(IdentifierLabel(l), t)) ++ ts}

    one | more
  }

  /**
    * The sumts method is a helper method which helps to build a list of element types for sum types. As with tuples, sum types
    * must have at least two types.
    *
    * This method helps to enforce that a sum type is made of at least two types.
    *
    * @return a parser of a list of types
    */
  def sumts: Parser[List[Type]] = {
    val one = Or ~ _type ~ RParen ^^ { case _ ~ t ~ _ => List(t)}
    val more = Or ~ _type ~ sumts ^^ { case _ ~ t ~ ts => List(t) ++ ts}

    one | more
  }

  /**
    * A helper method which assembles a comma-separated list of identifiers.
    *
    * @return a parser of a list of identifiers.
    */
  def identifierlist: Parser[List[Identifier]] = {
    val zero = RParen ^^ { case _ => List[Identifier]() }
    val more = Comma ~ identifier ~ identifierlist ^^ { case _ ~ id ~ ids => List(id) ++ ids }

    zero | more
  }

  /**
    * A helper method which helps to assemble a list of parameters and their types for function types.
    *
    * This method and the typelist method work together to allow a function type to have zero, one, or more parameters
    * of arbitrary types.
    *
    * @return
    */
  def typelisthelper: Parser[List[Type]] = {
    val zero = RParen ^^ { case _ => List[Type]() }
    val more = Comma ~ _type ~ typelisthelper ^^ { case _ ~ t ~ ts => t :: ts }

    more | zero
  }

  /**
    * A helper method which helps to assemble a list of parameters and their types for function types.
    *
    * This method allows for a function to have zero or one parameter types. If there are more parameter types, they
    * are collected in the typelisthelper method.
    *
    * @return
    */
  def typelist: Parser[List[Type]] = {
    val zero = RParen ^^ { case _ => List() }
    val one = _type ~ typelisthelper ^^ { case t ~ ts => List(t) ++ ts }

    zero | one
  }


  /**
    * This method returns a parser of types. Most of the responsibility is given to the typehelper method. The _type method
    * checks for types that would otherwise be left-recursive, then calls the typehelper method to check for all other types.
    *
    * @return a parser of types
    */
  def _type: Parser[Type] = {
    val optionaltype = typehelper ~ QuestionMark ^^ { case t ~ _ => SumType(List(t, NullType)) }

    optionaltype | typehelper
  }

  /**
    * The typehelper method does most of the checking for types.
    *
    * @return a parser or types
    */
  def typehelper: Parser[Type] = {
    val intt  =_Int ^^ { _ => IntType }
    val uintt = _UInt ^^ { _ => UIntType }
    val floatt = _Float ^^ { _ => FloatType }
    val boolt = _Bool ^^ { _ => BoolType }
    val stringt = _String ^^ { _ => StringType }
    val definedt = identifier ^^ { t => DefinedType(t) }
    val nullt = _Null ^^ { _ => NullType }

    val arrayt = _Array ~ LBracket ~ _type ~ RBracket ^^ { case _ ~ _ ~ et ~ _ => ArrayType(et) }
    val tuplet = LParen ~ _type ~ tuplets(1) ^^ { case _ ~ t ~ ts => TupleType(List(LabeledElement(IndexLabel(0), t)) ++ ts) }
    val labeledtuplet = LParen ~ identifier ~ Colon ~ _type ~ labeledtuplets ^^
      {case _ ~ l ~ _ ~ t ~ ts => TupleType(List(LabeledElement(IdentifierLabel(l), t)) ++ ts) }
    val sumt = LParen ~ _type ~ sumts ^^ { case _ ~ t ~ ts => SumType(List(t) ++ ts)}
    val enumt = _Enum ~ LParen ~ identifier ~ identifierlist ^^ { case _ ~ _ ~ id ~ ids => EnumType(id :: ids)}
    //Right parenthesis consumed by typelist function.
    val functiont = LParen ~ typelist ~ Arrow ~ _type ^^ { case _ ~ ts ~ _ ~ rt => FunctionType(ts, rt)}

    intt | uintt | floatt | boolt | stringt | definedt | functiont | nullt | arrayt | tuplet | labeledtuplet | sumt | enumt
  }



  //Helper methods for the expression and subexpression methods

  /**
    * Helps assemble a list of parameters for a defined or anonymous function.
    *
    * @return
    */
  def params: Parser[List[Param]] = {
    val more = identifier ~ Colon ~ _type ~ Comma ~ params ^^ { case name ~ _ ~ t ~ _ ~ p => List(Param(name, t)) ++ p }
    val last = identifier ~ Colon ~ _type ~ RParen ^^ { case name ~ _ ~ t ~ _ => List(Param(name, t))}
    val none = RParen ^^ { case _ => List() }

    none | last | more
  }

  /**
    * A defined function is a function definition, which states the parameters, return type, and function body of a given
    * function. A definedFunction is a type of expression, but is defined here for clarity and called from the subexpression
    * method.
    *
    * @return
    */
  def definedFunction: Parser[Expression] = {
    //RParen is explicitly left out as it is handled by the params function.
    Def ~ identifier ~ LParen ~ params ~ Arrow ~ _type ~ LCurlyBrace ~ expression ~ RCurlyBrace ^^
      { case _ ~ name ~ _ ~ ps ~ _ ~ t ~ _ ~ body ~ _ => FunctionDef(name, ps, t, body) }
  }

  /**
    * A helper method which helps to assemble a list of arguments for a function call.
    *
    * Arguments can be an arbitrary expression.
    *
    * @return
    */
  def args: Parser[List[Expression]] = {
    val more = expression ~ Comma ~ args ^^ { case e ~ _ ~ a => List(e) ++ a }
    val last = expression ~ RParen ^^ { case e ~ _ => List(e) }
    val none = RParen ^^ { case _ => List() }

    none | last | more
  }

  /**
    * A helper method which collects an arbitrary number of "else if" subexpressions for packaging into a single if
    * statement.
    *
    * @return
    */
  def elseifs: Parser[List[IfSubExpression]] = {
    val more = Else ~ If ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace ~ elseifs ^^
      { case _ ~ _ ~ condition ~ _ ~ body ~ _ ~ additional => List(IfSubExpression(condition, body)) ++ additional }
    val none = Else ^^ { case _ => List[IfSubExpression]() }

    more | none
  }

  /**
    * A helper method which collects an arbitrary number of "else if let" subexpressions for packaging into a single
    * if let statement.
    *
    * @return
    */
  def elseiflets: Parser[List[IfLetSubExpression]] = {
    val more = Else ~ If ~ Let ~ identifier ~ EqualsSign ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace ~ elseiflets ^^
      { case _ ~ _ ~ _ ~ id ~ _ ~ condition ~ _ ~ body ~ _ ~ additional => List(IfLetSubExpression(id, condition, body)) ++ additional }
    val none = Else ^^ { case _ => List[IfLetSubExpression]() }

    more | none
  }


  //This set of methods:
  // letCaseEntries
  // listCaseEntriesHelper
  // listCaseEntries
  // caseentries
  // are helper methods for the case construct.
  /**
    * This method helps collect all cases described under letCaseEntriesHelper into a single list.
    *
    * A default case at the end of the list is also allowed.
    *
    * @return
    */
  def letCaseEntries: Parser[List[CaseEntry]] = {
    val letce = Let ~ identifier ~ Colon ~ _type ~ Arrow ~ expression ~ letCaseEntries ^^
      { case _ ~ id ~ _ ~ t ~ _ ~ e ~ cases => List(CaseEntry(LetCasePattern(id, t), e)) ++ cases}

    val lastce = Let ~ identifier ~ Colon ~ _type ~ Arrow ~ expression ^^
      { case _ ~ id ~ _ ~ t ~ _ ~ e => List(CaseEntry(LetCasePattern(id, t), e))}

    val defaultce = Default ~ Arrow ~ expression ^^ { case _ ~ _ ~ e => List(CaseEntry(DefaultCasePattern, e))}

    letce | lastce | defaultce
  }

  /**
    * A helper method which helps assemble the enum values which correspond to this particular case.
    *
    * In a given case statement, it is allowed to list multiple enums for a single case, eg.
    * MONDAY, TUESDAY, WEDNESDAY -> dostuff()
    * This helps collect all of the options.
    *
    * @return
    */
  def listCaseEntriesHelper: Parser[List[Identifier]] = {
    val more = identifier ~ Comma ~ listCaseEntriesHelper ^^ { case id ~ _ ~ cases => List(id) ++ cases }
    val one = identifier ~ Arrow ^^ { case id ~ _ => List(id) }

    more | one
  }

  /**
    * This is a helper method which collects let-type case entries into a list.
    *
    * A case construct can be used to differentiate which type a sum type belongs to. Cases are constructed as
    * let x: Int -> foo(x)
    * let y: Float -> bar(y)
    *
    * Each of these is collected into a list.
    *
    * A default case at the end of the list is also allowed.
    *
    * @return
    */
  def listCaseEntries: Parser[List[CaseEntry]] = {
    val listce = listCaseEntriesHelper ~ expression ~ listCaseEntries ^^
      { case ids ~ e ~ ces => List(CaseEntry(ListCasePattern(ids), e)) ++ ces}
    val lastce = listCaseEntriesHelper ~ expression ^^
      { case ids ~ e => List(CaseEntry(ListCasePattern(ids), e))}

    val defaultce = Default ~ Arrow ~ expression ^^ { case _ ~ _ ~ e => List(CaseEntry(DefaultCasePattern, e))}

    listce | lastce | defaultce
  }

  /**
    * This helper method collects all case entries in a case statement into list. Note that there are two types of case
    * entries, list and let case entries, and a case statement can consist of only one, not both. The method uses the first
    * row to determine which type it is and delegates the responsibility of parsing the rest of the entries to
    * the listCaseEntries or letCaseEntries methods.
    *
    * All three methods allow the case construct to end with a default case.
    *
    * @return
    */
  def caseentries: Parser[List[CaseEntry]] = {
    val letce = Let ~ identifier ~ Colon ~ _type ~ Arrow ~ expression ~ letCaseEntries ^^
      { case _ ~ id ~ _ ~ t ~ _ ~ e ~ ces => List(CaseEntry(LetCasePattern(id, t), e)) ++ ces}

    //The Arrow token is consumed by the listCaseEntriesHelper method
    val listce = listCaseEntriesHelper ~ expression ~ listCaseEntries ^^
      { case ids ~ e ~ ces => List(CaseEntry(ListCasePattern(ids), e)) ++ ces }

    val defaultce = Default ~ Arrow ~ expression ^^ { case _ ~ _ ~ e => List(CaseEntry(DefaultCasePattern, e))}

    letce | listce | defaultce
  }

  /**
    * Expressions make up the core of an assertions functionality. This method works with the subexpression method to make
    * up an assertion's functional body.
    *
    * @return
    */
  def expression: Parser[Expression] = {

    //val fcall2args = subexpression ~ identifier ~ expression ^^ { case e1 ~ name ~ e2 => FunctionCall(name, List(e1, e2)) }
    val optionalexpr = subexpression ~ QuestionMark ^^ { case e ~ _ => OptionalExpression(e) }
    val ascription = subexpression ~ As ~ _type ^^ { case e ~ _ ~ t => Ascription(e, t) }

    //Hypothetically, the i.toInt here could fail with an exception, but the regex which captures the string for IntConstant
    //ensures that it will be convertible to an integer. If toInt throws an exception, modify the regex in the lexer.
    val indexedtupleselect = subexpression ~ Dot ~ intconst ^^ { case e ~ _ ~ IntConstant(i) => Selection(e, IndexLabel(i.toInt)) }
    val labeledtupleselect = subexpression ~ Dot ~ identifier ^^ { case e ~ _ ~ id => Selection(e, IdentifierLabel(id))}

    optionalexpr | indexedtupleselect | indexedtupleselect | labeledtupleselect | ascription | subexpression
  }

  /**
    * Subexpression defines expressions that do have require an expression or subexpression as the first element in their pattern.
    *
    * This helps to handle left recursion.
    *
    * @return a parser of expressions.
    */
  def subexpression: Parser[Expression] = {

    //The final else is consumed by the elseifs function as a delimiter.
    val ifexpr = If ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace ~ elseifs ~ LCurlyBrace ~ expression ~ RCurlyBrace ^^
      { case _ ~ condition ~ _ ~ truebody ~ _ ~ eifs ~ _ ~ falsebody ~ _ =>
        IfExpression(IfSubExpression(condition, truebody) :: eifs, falsebody)}

    //The final else is consumed by the elseiflets function as a delimiter.
    val ifletexpr = If ~ Let ~ identifier ~ EqualsSign ~ expression ~ LCurlyBrace ~ expression ~ RCurlyBrace ~
      elseiflets ~ LCurlyBrace ~ expression ~ RCurlyBrace ^^
      { case _ ~ _ ~ id1 ~ _ ~ e1 ~ _ ~ b1 ~ _ ~ eifls ~ _ ~ bf ~ _ =>
        IfLetExpression(IfLetSubExpression(id1, e1, b1) :: eifls, bf)}

    val caseexpr = Case ~ expression ~ LCurlyBrace ~ caseentries ~ RCurlyBrace ^^
      { case _ ~ e ~ _ ~ cases ~ _ => CaseExpression(e, cases) }

    //Right parenthesis is purposefully missing as it is consumed by the args function as a delimiter.
    val fcall = identifier ~ LParen ~ args ^^ { case name ~ _ ~ ps => FunctionCall(name, ps) }

    //val fcall1arg = identifier ~ expression ^^ { case name ~ e => FunctionCall(name, List(e)) }

    val anonymousfunc = LParen ~ params ~ Arrow ~ expression ^^
      { case _ ~ ps ~ _ ~ e => AnonymousFunction(ps, e) }

    val letexpr = Let ~ identifier ~ EqualsSign ~ expression ~ In.? ~ expression ^^
      { case _ ~ variable ~ _ ~ e1 ~ _ ~ e2 => LetExpression(variable, e1, e2) }

    val parentheticalexpr = LParen ~ expression ~ RParen ^^ { case _ ~ e ~ _ => ParentheticalExpression(e)}

    val idexpr = identifier ^^ { id => VariableExpression(id) }

    val definedfuncexpr = definedFunction

    val boolc = boolconst ^^ { case bc => BoolConstantExpr(bc) }
    val stringc = stringconst ^^ { case sc => StringConstantExpr(sc) }
    val intc = intconst ^^ { case ic => IntConstantExpr(ic) }
    val floatc = floatconst ^^ { case fc => FloatConstantExpr(fc) }
    val nullc = NullValue ^^ { case _ => NullExpression }

    val fcall2args = { fcall | idexpr | boolc | stringc | intc | floatc | nullc } ~
      identifier ~ expression ^^
      { case e1 ~ name ~ e2 => FunctionCall(name, List(e1, e2)) }


    ifexpr | ifletexpr | caseexpr | letexpr | anonymousfunc | parentheticalexpr | definedfuncexpr | fcall2args | fcall |
      idexpr | boolc | stringc | intc | floatc | nullc
  }

  /**
    * The entry point for the parsing phase (AST creation).
    *
    * @param tokens A list with elements of type Token which will be parsed into an AST
    * @return
    */
  def apply(tokens: List[Token]): Assertion = {
    assertion(new TokenReader(tokens)) match {
      case NoSuccess(msg, _) => throw new ParsingException(msg)
      case Success(result, _) => result
    }
  }
}