package com.iz2use.express.ast

case object AbstractEntityDeclaration extends SupertypeConstraint
case class AbstractSupertypeDeclaration(SubtypeConstraint: Option[SupertypeExpression]) extends SupertypeConstraint
case class AggregateInitializer(items: Seq[Expression]) extends Expression
case class AggregateType(label: Option[String], of: ParameterType) extends GeneralizedType
sealed trait AggregationType[T <: AggregationTypeLevel] extends ConcreteType with GeneralizedType {
  def of: T
}
sealed trait AggregationTypeLevel extends RootType
case class ArrayType[T <: AggregationTypeLevel](bounds: Option[Bounds], optional: Boolean, unique: Boolean, of: T) extends AggregationType[T]
case class AttributeQualifier(name: String) extends Qualifier

case class BagType[T <: AggregationTypeLevel](bounds: Option[Bounds], of: T) extends AggregationType[T]
case class BasedOnEnumeration(name: String, additionalItems: Seq[EnumerationItem])
case class BinaryLiteral(value: collection.BitSet) extends Literal
case class BinaryType(width: Option[Width]) extends SimpleType
case object BooleanType extends SimpleType
case class Bounds(lowerBound: NumericExpression, upperBound: NumericExpression)
sealed trait BuiltInConstant extends ConstantFactor with QualifiableFactor
object BuiltInConstant {
  case object E extends BuiltInConstant
  case object PI extends BuiltInConstant
  case object Self extends BuiltInConstant
  case object Unknown extends BuiltInConstant
}
sealed trait BuiltInFunction extends FunctionOrEntityConstructor
object BuiltInFunction {
  case object Abs extends BuiltInFunction
  case object Acos extends BuiltInFunction
  case object Asin extends BuiltInFunction
  case object Atan extends BuiltInFunction
  case object BLength extends BuiltInFunction
  case object Cos extends BuiltInFunction
  case object Exists extends BuiltInFunction
  case object Exp extends BuiltInFunction
  case object Format extends BuiltInFunction
  case object HiBound extends BuiltInFunction
  case object HiIndex extends BuiltInFunction
  case object Length extends BuiltInFunction
  case object LoBound extends BuiltInFunction
  case object LoIndex extends BuiltInFunction
  case object Log extends BuiltInFunction
  case object Log2 extends BuiltInFunction
  case object Log10 extends BuiltInFunction
  case object Nvl extends BuiltInFunction
  case object Odd extends BuiltInFunction
  case object RolesOf extends BuiltInFunction
  case object Sin extends BuiltInFunction
  case object SizeOf extends BuiltInFunction
  case object Sqrt extends BuiltInFunction
  case object Tan extends BuiltInFunction
  case object TypeOf extends BuiltInFunction
  case object UsedIn extends BuiltInFunction
  case object Value extends BuiltInFunction
  case object ValueIn extends BuiltInFunction
  case object ValueUnique extends BuiltInFunction
}
sealed trait BuiltInProcedure extends Procedure
object BuiltInProcedure {
  case object Insert extends BuiltInProcedure
  case object Remove extends BuiltInProcedure
}

sealed trait ConcreteType extends InstantiableType with UnderlyingType
sealed trait ConstantFactor
sealed trait ConstructedType extends UnderlyingType with RootType

case class DomainRule(name: Option[String], expr: Expression)

//case class EntityConstructor(entityRef: String, items: Seq[Expression]) extends Expression
case class EnumerationItem(name: String)
case class EnumerationReference(typeRef: Option[String], enumerationRef: String) extends Expression
case class EnumerationType(extensible: Boolean, items: Option[Either[Seq[EnumerationItem], BasedOnEnumeration]]) extends ConstructedType
sealed trait Expression extends NumericExpression with LogicalExpression

sealed trait FunctionOrEntityConstructor
case class FunctionCallOrEntityConstructor(functionOrEntity: FunctionOrEntityConstructor, parameterList: Seq[Expression]) extends QualifiableFactor with Expression

sealed trait GeneralizedType extends ParameterType
case class GenericEntityType(label: Option[String]) extends GeneralizedType
case class GenericType(label: Option[String]) extends GeneralizedType
case class GroupQualifier(name: String) extends Qualifier

case class IndexQualifier(index0: NumericExpression, index1: Option[NumericExpression]) extends Qualifier
sealed trait InstantiableType extends AggregationTypeLevel
case class IntegerLiteral(value: String) extends NumberLiteral
case object IntegerType extends SimpleType
case class Interval(lowBound: Expression, lowInclusive: Boolean, item: Expression, highInclusive: Boolean, highBound: Expression) extends Expression

case class ListType[T <: AggregationTypeLevel](bounds: Option[Bounds], unique: Boolean, of: T) extends AggregationType[T]
sealed trait Literal extends Primary
sealed trait LogicalExpression
case class LogicalLiteral(value: Option[Boolean]) extends Literal
case object LogicalType extends SimpleType

case class MultipleOperation[Op <: Operator](initialValue: Expression, steps: Seq[(Op, Expression)]) extends Expression

sealed trait NumberLiteral extends Literal
case object NumberType extends SimpleType
sealed trait NumericExpression

case class Parameter(name: String, tpe: ParameterType)
sealed trait ParameterType extends AggregationTypeLevel
sealed trait Primary extends Expression
sealed trait Procedure
case class ProcedureParameters(variable: Boolean, parameters: Seq[Parameter])

sealed trait QualifiableFactor extends Primary
case class QualifiedApply(qualifiable: QualifiableFactor, qualifier: Seq[Qualifier]) extends Primary
sealed trait Qualifier
case class QueryExpression(variableId: String, aggregatorSource: Expression, logicalExpression: LogicalExpression) extends Expression

case class RealLiteral(value: String) extends NumberLiteral
case class RealType(precision: Option[NumericExpression]) extends SimpleType
case class RenamedResource(resourceRef: String, as: String)
case class RenamedType(namedType: String, as: String)
case class Repetition(expression: Expression, count: NumericExpression) extends Expression
sealed trait RootType

case class SelectTypeExtensible(generic: Boolean)
case class SelectTypeFromBasedOn(name: String, additionalItems: Seq[String])
case class SelectType(extensible: Option[SelectTypeExtensible], from: Option[Either[Seq[String], SelectTypeFromBasedOn]]) extends ConstructedType
case class SetType[T <: AggregationTypeLevel](bounds: Option[Bounds], of: T) extends AggregationType[T]
sealed trait SimpleType extends ConcreteType with ParameterType
case class SingleOperation[Op <: Operator](lhs: Expression, op: Op, rhs: Expression) extends Expression
case class StringLiteral(value: String) extends Literal
case class StringType(width: Option[Width]) extends SimpleType
case class SubtypeConstraint(entities: Seq[String])
sealed trait SupertypeConstraint
sealed trait SupertypeExpression
case class SupertypeOneOf(items: Seq[SupertypeExpression]) extends SupertypeExpression
case class SupertypeProduct(items: Seq[SupertypeExpression]) extends SupertypeExpression
case class SupertypeRule(subtypeConstraint: SupertypeExpression) extends SupertypeConstraint
case class SupertypeSum(items: Seq[SupertypeExpression]) extends SupertypeExpression

case class UnaryOperation(op: UnaryOp, expr: Expression) extends Expression
sealed trait UnderlyingType
case class UserDefinedConstant(name: String) extends ConstantFactor
case class UserDefinedEntity(name: String) extends InstantiableType with SupertypeExpression
case class UserDefinedEntityOrType(name: String) extends ParameterType
case class UserDefinedFactor(name: String) extends QualifiableFactor
case class UserDefinedFunctionOrEntityConstructor(name: String) extends FunctionOrEntityConstructor
case class UserDefinedProcedure(name: String) extends Procedure
case class UserDefinedType(name: String) extends ConcreteType

case class WhereClause(clauses: Seq[DomainRule])
case class Width(width: NumericExpression, fixed: Boolean)