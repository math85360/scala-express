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
  sealed trait Node
  case class Schema(id: String, version: Option[StringLiteral], body: Seq[SchemaBody]) extends Node
  sealed trait SchemaBody
  sealed trait InterfaceSpecification extends SchemaBody
  case class ReferenceClause(schemaRef: String, imports: Seq[NamedTypeOrRename]) extends Node with InterfaceSpecification
  
  sealed trait AlgorithmHeadPart
  case class UseClause(schemaRef: String, imports: Seq[NamedTypeOrRename]) extends Node with InterfaceSpecification
  case class ConstantDeclaration(constantId: String, tpe: InstantiableType, expression: Expression) extends Node with SchemaBody with AlgorithmHeadPart
  case class Declaration() extends Node with SchemaBody with AlgorithmHeadPart
  case class Rule(ruleId: String, entityRefs: Seq[EntityRef], header: Seq[AlgorithmHeadPart], body: Seq[Statement], whereClause: WhereClause) extends Node with SchemaBody
  case class LocalDeclaration(variableNames: Seq[String], parameter: ParameterType, defaultExpression: Option[Expression]) extends AlgorithmHeadPart
  sealed trait Statement
  

}