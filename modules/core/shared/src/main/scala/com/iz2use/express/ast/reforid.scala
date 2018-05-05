package com.iz2use.express.ast

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
}
