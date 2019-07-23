package granolap

sealed trait Program
case class Assertion(header: List[Statement], body: List[Expression]) extends Program


sealed trait Statement
case class ImportStatement(variable: String, renamed: Identifier) extends Statement
case class TypedefStatement(variable: String, castedtype: Type) extends Statement


sealed trait DefinedFunction
case class FunctionDef(name: String, args: List[(String, Type)], returnType: Type, body: Expression) extends DefinedFunction


sealed trait Expression
case class IfExpression(cases: List[(Expression, Expression)], elseBody: Expression) extends Expression
case class IfLetExpression(cases: List[(String, Expression, Expression)], elseBody: Expression) extends Expression
case class CaseExpression(variable: String, cases: List[(CasePattern, Expression)]) extends Expression
case class FunctionCall(name: String, params: List[Expression]) extends Expression
case class AnonymousFunction(args: List[(String, Type)], body: Expression) extends Expression
case class LetExpression(variable: String, e1: Expression, e2: Expression) extends Expression
case class OptionalExpression(expression: Expression) extends Expression
case class Ascription(expression: Expression, ascriptedType: Type) extends Expression
case class Selection(expression: Expression, item: IntConstant) extends Expression
case object NullExpression extends Expression

sealed trait Constant extends Expression
case class FloatConstantExpr(value: String) extends Constant
case class IntConstantExpr(value: String) extends Constant
case class BoolConstantExpr(value: String) extends Constant
case class StringConstantExpr(value: String) extends Constant


sealed trait CasePattern
case class LetCasePattern(variable: String, variableType: Type) extends CasePattern
case class ListCasePattern(options: List[String])


sealed trait Type
case object IntType extends Type
case object UIntType extends Type
case object FloatType extends Type
case object BoolType extends Type
case object StringType extends Type
case class ArrayType(elements: Type) extends Type
case class TupleType(elements: List[(String, Type)]) extends Type
case class SumType(options: List[(String, Type)]) extends Type
case class EnumType(options: List[String])