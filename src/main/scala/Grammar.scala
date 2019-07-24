package granolap

sealed trait Program
case class Assertion(header: List[Statement], body: List[Expression]) extends Program


sealed trait Statement
case class ImportStatement(variable: Identifier, renamed: Identifier) extends Statement
case class TypedefStatement(variable: Identifier, castedtype: Type) extends Statement


case class FunctionDef(name: Identifier, params: List[Param], returnType: Type, body: Expression) extends Expression

case class Param(varname: Identifier, vartype: Type)


sealed trait Expression
case class IfExpression(cases: List[IfSubExpression], elseBody: Expression) extends Expression
case class IfLetExpression(cases: List[IfLetSubExpression], elseBody: Expression) extends Expression
case class CaseExpression(variable: Identifier, cases: List[CaseEntry]) extends Expression
case class FunctionCall(name: Identifier, params: List[Expression]) extends Expression
case class AnonymousFunction(args: List[(Identifier, Type)], body: Expression) extends Expression
case class LetExpression(variable: Identifier, e1: Expression, e2: Expression) extends Expression
case class ParentheticalExpression(expression: Expression) extends Expression
case class OptionalExpression(expression: Expression) extends Expression
case class Ascription(expression: Expression, ascriptedType: Type) extends Expression
case class Selection(expression: Expression, item: IntConstant) extends Expression
case class VariableExpression(variable: Identifier) extends Expression
case object NullExpression extends Expression

case class IfSubExpression(condition: Expression, body: Expression)
case class IfLetSubExpression(variable: Identifier, condition: Expression, body: Expression)

sealed trait Constant extends Expression
case class FloatConstantExpr(value: FloatConstant) extends Constant
case class IntConstantExpr(value: IntConstant) extends Constant
case class BoolConstantExpr(value: BoolConstant) extends Constant
case class StringConstantExpr(value: StringConstant) extends Constant


case class CaseEntry(pattern: CasePattern, expression: Expression)

sealed trait CasePattern
case class LetCasePattern(variable: Identifier, variableType: Type) extends CasePattern
case class ListCasePattern(options: List[Identifier]) extends CasePattern
case object DefaultCasePattern extends CasePattern


sealed trait Type
case object IntType extends Type
case object UIntType extends Type
case object FloatType extends Type
case object BoolType extends Type
case object StringType extends Type
case class DefinedType(name: Identifier) extends Type
case class ArrayType(elements: Type) extends Type
case class TupleType(elements: List[(Identifier, Type)]) extends Type
case class SumType(options: List[(Identifier, Type)]) extends Type
case class EnumType(options: List[Identifier])