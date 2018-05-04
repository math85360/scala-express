package com.iz2use.express

package object ast {
  sealed trait Alternative
  //sealed trait |[+A, +B] extends Alternative
  case class |[+A, +B <: Alternative](head: A, tail: B) extends Alternative
  sealed trait PipeNil extends Alternative
  case object PipeNil extends PipeNil
  //case class OneOf[
  //case class |[+A, +B](head: A, tail: B)
  object | {
    implicit def fromA[A](value: A): |[A, PipeNil] = |(value, PipeNil)
    implicit def fromB[B <: Alternative](value: B): |[PipeNil, B] = |(PipeNil, value)
  }
  
  
  
  

  
  sealed trait Statement
  case class AliasStatement(name: String, source: String, path: Seq[ast.Qualifier], body: Seq[Statement]) extends Statement
  case class AssignmentStatement(target: String, path: Seq[ast.Qualifier], expression: Expression) extends Statement
  case class Case(conditions: Seq[Expression], body: Statement)
  case class CaseStatement(selector: Expression, cases: Seq[Case], otherwise: Option[Statement]) extends Statement
  case class CompoundStatement(body: Seq[Statement]) extends Statement
  case object EscapeStatement extends Statement
  case class IfStatement(condition: LogicalExpression, ifPass: Seq[Statement], ifFailed: Seq[Statement]) extends Statement
  case object NullStatement extends Statement
  case class ProcedureCallStatement(procedure: Procedure, parameterList: Seq[Expression]) extends Statement
  case class IncrementControl(target: String, from: NumericExpression, to: NumericExpression, increment: Option[NumericExpression])
  case class RepeatStatement(incrementControl: Option[IncrementControl], whileControl: Option[LogicalExpression], untilControl: Option[LogicalExpression], body: Seq[Statement]) extends Statement
  case class ReturnStatement(result: Option[Expression]) extends Statement
  case object SkipStatement extends Statement
  sealed trait SupertypeConstraint
  sealed trait SubtypeConstraint
  //case class AbstractEntityDeclaration()
  sealed trait EntityBody
  
  sealed trait Attribute
  case class ExplicitAttribute(names: AttributeName, optional: Boolean, tpe: ParameterType) 
  case class DerivedAttribute(names: AttributeName, tpe: ParameterType, value: Expression) 
  case class InverseAttribute(names: AttributeName, tpe: Option[InverseAggregateType], source: String, entity: Option[String], attribute: String) 
  case class UniqueClause(name: Option[String], source: Seq[UniqueSource])
  sealed trait UniqueSource
  case class ReferencedAttribute(attribute: String) extends UniqueSource
  sealed trait AttributeName
  case class SimpleAttributeName(name: String) extends AttributeName
  case class RedeclaredAttribute(source: QualifiedAttribute, renamed: Option[String]) extends AttributeName
  case class InverseAggregateType(unique: Boolean, bounds: Option[Bounds])
  case class QualifiedAttribute(group: String, attribute: String) extends UniqueSource
  
  
  
  //type ResourceRef =  ConstantRef <: BaseRef
  //case class Ref(value: Id)
  //case class Id(name: String)
}