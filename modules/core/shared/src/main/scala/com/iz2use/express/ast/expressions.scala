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

case class BuiltInFunction(name: String) extends Function
case class BuiltInConstant(name: String) extends ConstantFactor
sealed trait QualifiableFactor
//case class AttributeRef(id: String) extends QualifiableFactor
sealed trait ConstantFactor extends QualifiableFactor
sealed trait Qualifier
case class AttributeQualifier(ref: AttributeRef) extends Qualifier
case class GroupQualifier(entityRef: EntityRef) extends Qualifier
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

sealed trait InstantiableType
//case class EntityRef(entityId: String) extends InstantiableType with QualifiableFactor with NamedType
sealed trait ConcreteType extends InstantiableType
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
case class EntityConstructor(entityRef: EntityRef, items: Seq[Expression]) extends Expression
case class EnumerationReference(typeRef: Option[TypeRef], enumerationRef: EnumerationRef) extends Expression
//case class EnumerationRef(enumeration: String)

case object RepetitionOp extends Operator

sealed trait NamedTypeOrRename
case class RenamedType(namedType: NamedType, as: String) extends NamedTypeOrRename
sealed trait NamedType extends NamedTypeOrRename with ParameterType
sealed trait ResourceOrRename
case class RenamedResource(resourceRef: ResourceRef, as: String) extends ResourceOrRename
//case class ResourceRef(resourceId: String) extends ResourceOrRename

sealed trait BaseRef {
  type Aux <: BaseId
}
trait AttributeRef extends BaseRef{ type Aux = AttributeId }
trait ConstantRef extends BaseRef{ type Aux = ConstantId }
trait EntityRef extends BaseRef{ type Aux = EntityId }
trait EnumerationRef extends BaseRef{ type Aux = EnumerationId }
trait FunctionRef extends BaseRef{ type Aux = FunctionId }
trait GeneralRef extends BaseRef{ type Aux = GeneralId }
trait ParameterRef extends BaseRef{ type Aux = ParameterId }
trait ProcedureRef extends BaseRef{ type Aux = ProcedureId }
trait ResourceRef extends BaseRef{ type Aux = ResourceId }
trait RuleLabelRef extends BaseRef{ type Aux = RuleLabelId }
trait RuleRef extends BaseRef{ type Aux = RuleId }
trait SchemaRef extends BaseRef{ type Aux = SchemaId }
trait SubtypeConstraintRef extends BaseRef{ type Aux = SubtypeConstraintId }
trait TypeLabelRef extends BaseRef{ type Aux = TypeLabelId }
trait TypeRef extends BaseRef{ type Aux = TypeId }
trait VariableRef extends BaseRef{ type Aux = VariableId }
case class Ref[+T <: BaseRef](id: T)
sealed trait BaseId
trait AttributeId extends BaseId
trait ConstantId extends BaseId
trait EntityId extends BaseId
trait EnumerationId extends BaseId
trait FunctionId extends BaseId
trait GeneralId extends BaseId
trait ParameterId extends BaseId
trait ProcedureId extends BaseId
trait ResourceId extends BaseId
trait RuleLabelId extends BaseId
trait RuleId extends BaseId
trait SchemaId extends BaseId
trait SubtypeConstraintId extends BaseId
trait TypeLabelId extends BaseId
trait TypeId extends BaseId
trait VariableId extends BaseId
case class Id[+T <: BaseId](value: String)
  /*extends AttributeRef
  with ConstantRef
  with EntityRef
  with EnumerationRef
  with FunctionRef
  with GeneralRef
  with ParameterRef
  with 
*/