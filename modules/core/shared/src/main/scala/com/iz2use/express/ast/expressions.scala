package com.iz2use.express.ast

sealed trait NumericExpression
sealed trait LogicalExpression
sealed trait Expression extends NumericExpression with LogicalExpression

sealed trait ParameterType
sealed trait GeneralizedType extends ParameterType

case class QueryExpression(variableId: String, aggregatorSource: Expression, logicalExpression: LogicalExpression) extends Expression
case class Bounds(lowerBound: NumericExpression, upperBound: NumericExpression)
case class DomainRule(name: Option[String], expr: Expression)
case class WhereClause(clauses: Seq[DomainRule])
sealed trait AddLikeOp extends Operator
case object AdditionOp extends AddLikeOp
case object SubtractionOp extends AddLikeOp
case object OrOp extends AddLikeOp
case object XorOp extends AddLikeOp

sealed trait MultiplicationLikeOp extends Operator
case object MultiplicationOp extends MultiplicationLikeOp
case object DivisionOp extends MultiplicationLikeOp
case object ModuloOp extends MultiplicationLikeOp
case object AndOp extends MultiplicationLikeOp
case object OrElseOp extends MultiplicationLikeOp

sealed trait Operator
sealed trait RelOpExtended extends Operator
sealed trait RelOp extends RelOpExtended
case object LT extends RelOp
case object GT extends RelOp
case object LE extends RelOp
case object GE extends RelOp
case object NE extends RelOp
case object EQ extends RelOp
case object TE extends RelOp
case object TN extends RelOp
case object IN extends RelOpExtended
case object LIKE extends RelOpExtended

sealed trait UnaryOp extends Operator
case object PositiveOp extends UnaryOp
case object NegativeOp extends UnaryOp
case object NotOp extends UnaryOp

case object PowerOp extends Operator

sealed trait Procedure
sealed trait BuiltInProcedure extends Procedure
object BuiltInProcedure {
  case object Insert extends BuiltInProcedure
  case object Remove extends BuiltInProcedure
}
case class UserDefinedProcedure(name: String) extends Procedure
sealed trait BuiltInFunction extends Function
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
case class UserDefinedFunction(name: String) extends Function
sealed trait BuiltInConstant extends ConstantFactor with QualifiableFactor
object BuiltInConstant {
  case object E extends BuiltInConstant
  case object PI extends BuiltInConstant
  case object Self extends BuiltInConstant
  case object Unknown extends BuiltInConstant
}
case class UserDefinedConstant(name: String) extends ConstantFactor
sealed trait QualifiableFactor
case class UserDefinedFactor(name: String) extends QualifiableFactor
//case class AttributeRef(id: String) extends QualifiableFactor
sealed trait ConstantFactor
sealed trait Qualifier
case class AttributeQualifier(name: String) extends Qualifier
case class GroupQualifier(name: String) extends Qualifier
//case class ConstantRef(name: String) extends ConstantFactor
case class IndexQualifier(index0: NumericExpression, index1: Option[NumericExpression]) extends Qualifier
case class Interval(lowBound: Expression, lowInclusive: Boolean, item: Expression, highInclusive: Boolean, highBound: Expression) extends Expression
case class FunctionCall(function: Function, parameterList: Seq[Expression]) extends QualifiableFactor
//case class GeneralRef(id: String) extends QualifiableFactor
//case class VariableRef(id: String) extends QualifiableFactor
//case class ParameterRef(id: String) extends QualifiableFactor
sealed trait Function
//case class FunctionRef(name: String) extends Function
sealed trait Primary extends Expression

case class MultipleOperation[Op <: Operator](initialValue: Expression, steps: Seq[(Op, Expression)]) extends Expression
case class SingleOperation[Op <: Operator](lhs: Expression, op: Op, rhs: Expression) extends Expression
case class UnaryOperation(op: UnaryOp, expr: Expression) extends Expression

sealed trait UnderlyingType
sealed trait ConstructedType extends UnderlyingType

case class EnumerationItem(name: String)
case class BasedOnEnumeration(name: String, additionalItems: Seq[EnumerationItem])
case class EnumerationType(extensible: Boolean, items: Option[Either[Seq[EnumerationItem], BasedOnEnumeration]]) 
sealed trait InstantiableType
case class UserDefinedEntity(name: String) extends InstantiableType
//case class EntityRef(entityId: String) extends InstantiableType with QualifiableFactor with NamedType
sealed trait ConcreteType extends InstantiableType with UnderlyingType
case class UserDefinedType(name: String) extends ConcreteType
case class UserDefinedEntityOrType(name: String) extends ParameterType
sealed trait AggregationType[T] extends ConcreteType with GeneralizedType
case class ArrayType[T](bounds: Option[Bounds], optional: Boolean, unique: Boolean, tpe: T) extends AggregationType[T]
case class BagType[T](bounds: Option[Bounds], of: T) extends AggregationType[T]
case class ListType[T](bounds: Option[Bounds], unique: Boolean, tpe: T) extends AggregationType[T]
case class SetType[T](bounds: Option[Bounds], of: T) extends AggregationType[T]

case class GenericEntityType(label: Option[String]) extends GeneralizedType
case class GenericType(label: Option[String]) extends GeneralizedType
case class AggregateType(label: Option[String], of: ParameterType) extends GeneralizedType

case class QualifiedApply(qualifiable: QualifiableFactor, qualifier: Seq[Qualifier]) extends Primary

sealed trait SimpleType extends ConcreteType with ParameterType
case class BinaryType(width: Option[Width]) extends SimpleType
case object BooleanType extends SimpleType
case object IntegerType extends SimpleType
case object LogicalType extends SimpleType
case object NumberType extends SimpleType
case class RealType(precision: Option[NumericExpression]) extends SimpleType
case class StringType(width: Option[Width]) extends SimpleType
//case class TypeRef(typeId: String) extends ConcreteType with NamedType
case class Width(width: NumericExpression, fixed: Boolean)

sealed trait Literal extends Primary
sealed trait NumberLiteral extends Literal
case class BinaryLiteral(value: collection.BitSet) extends Literal
case class IntegerLiteral(value: String) extends NumberLiteral
case class LogicalLiteral(value: Option[Boolean]) extends Literal
case class RealLiteral(value: String) extends NumberLiteral
case class StringLiteral(value: String) extends Literal
case class AggregateInitializer(items: Seq[Expression]) extends Expression
case class EntityConstructor(entityRef: String, items: Seq[Expression]) extends Expression
case class EnumerationReference(typeRef: Option[String], enumerationRef: String) extends Expression
//case class EnumerationRef(enumeration: String)

case object RepetitionOp extends Operator

case class RenamedType(namedType: String, as: String)
//sealed trait NamedType extends NamedTypeOrRename with ParameterType
case class RenamedResource(resourceRef: String, as: String)

//case class ResourceRef(resourceId: String) extends ResourceOrRename

sealed trait BaseRefOrId
sealed trait BaseRef extends BaseRefOrId {
  type Id <: BaseId
}
trait AttributeRef extends BaseRef { type Id = AttributeId }
trait ConstantRef extends BaseRef { type Id = ConstantId }
trait EntityRef extends BaseRef { type Id = EntityId }
trait EnumerationRef extends BaseRef { type Id = EnumerationId }
trait FunctionRef extends BaseRef { type Id = FunctionId }
trait GeneralRef extends BaseRef { type Id = GeneralId }
trait ParameterRef extends BaseRef { type Id = ParameterId }
trait ProcedureRef extends BaseRef { type Id = ProcedureId }
//trait ResourceRef extends BaseRef { type Id = ResourceId }
trait RuleLabelRef extends BaseRef { type Id = RuleLabelId }
trait RuleRef extends BaseRef { type Id = RuleId }
trait SchemaRef extends BaseRef { type Id = SchemaId }
trait SubtypeConstraintRef extends BaseRef { type Id = SubtypeConstraintId }
trait TypeLabelRef extends BaseRef { type Id = TypeLabelId }
trait TypeRef extends BaseRef { type Id = TypeId }
trait VariableRef extends BaseRef { type Id = VariableId }

sealed trait BaseId extends BaseRefOrId {
  type Ref <: BaseRef
}
trait AttributeId extends BaseId { type Ref = AttributeRef }
trait ConstantId extends BaseId { type Ref = ConstantRef }
trait EntityId extends BaseId { type Ref = EntityRef }
trait EnumerationId extends BaseId { type Ref = EnumerationRef }
trait FunctionId extends BaseId { type Ref = FunctionRef }
trait GeneralId extends BaseId { type Ref = GeneralRef }
trait ParameterId extends BaseId { type Ref = ParameterRef }
trait ProcedureId extends BaseId { type Ref = ProcedureRef }
//trait ResourceId extends BaseId { type Ref = ResourceRef }
trait RuleLabelId extends BaseId { type Ref = RuleLabelRef }
trait RuleId extends BaseId { type Ref = RuleRef }
trait SchemaId extends BaseId { type Ref = SchemaRef }
trait SubtypeConstraintId extends BaseId { type Ref = SubtypeConstraintRef }
trait TypeLabelId extends BaseId { type Ref = TypeLabelRef }
trait TypeId extends BaseId { type Ref = TypeRef }
trait VariableId extends BaseId { type Ref = VariableRef }
class RefOrId[T <: BaseRefOrId](val name: String)
object RefOrId {
  def apply[T <: BaseId](name: String): RefOrId[T] =
    new RefOrId[T](name)
  def apply[T <: BaseRef](id: RefOrId[_]) =
    new RefOrId[T](id.name)

  //implicit def toString[T](value: RefOrId[T]): String = value.name
  //implicit def toBase[T <: BaseRefOrId](value: RefOrId[T]): T = null.asInstanceOf[T]
}
//case class Id[+T <: BaseId](value: String)
  /*extends AttributeRef
  with ConstantRef
  with EntityRef
  with EnumerationRef
  with FunctionRef
  with GeneralRef
  with ParameterRef
  with 
*/

case class Parameter(name: String, tpe: ParameterType)
case class ProcedureParameters(variable: Boolean, parameters: Seq[Parameter])