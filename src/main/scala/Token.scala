package granolap

sealed trait Token
case class Identifier(name: String) extends Token
case class IntConstant(value: String) extends Token
case class FloatConstant(value: String) extends Token
case class StringConstant(value: String) extends Token
case class BoolConstant(value: String) extends Token
case object If extends Token
case object Else extends Token
case object Let extends Token
case object Case extends Token
case object Default extends Token
case object Import extends Token
case object Typedef extends Token
case object As extends Token
case object Def extends Token
case object _Int extends Token
case object _UInt extends Token
case object _Float extends Token
case object _String extends Token
case object _Bool extends Token
case object _Enum extends Token
case object EnumValue extends Token
case object _Null extends Token
case object NullValue extends Token
case object Arrow extends Token
case object EqualsSign extends Token
case object NewLine extends Token
case object LParen extends Token
case object RParen extends Token
case object LCurlyBrace extends Token
case object RCurlyBrace extends Token
case object LBracket extends Token
case object RBracket extends Token
case object Comma extends Token
case object In extends Token
case object Colon extends Token
case object Dot extends Token
case object QuestionMark extends Token