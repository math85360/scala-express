package com.iz2use.express.ast

//sealed trait Node
sealed trait SchemaBody
sealed trait InterfaceSpecification extends SchemaBody
sealed trait Declaration extends SchemaBody with AlgorithmHeadPart
sealed trait AlgorithmHeadPart

case class ConstantDeclaration(constantId: String, tpe: InstantiableType, expression: Expression) extends SchemaBody with AlgorithmHeadPart

case class EntityDeclaration(
  name:              String,
  supertype:         Option[SupertypeConstraint],
  subtype:           Option[SubtypeConstraint],
  attributes:        Seq[ExplicitAttribute],
  derivedAttributes: Seq[DerivedAttribute],
  inverseAttributes: Seq[InverseAttribute],
  uniqueClause:      Seq[UniqueClause],
  whereClause:       Option[WhereClause]) extends Declaration

case class FunctionDeclaration(name: String, parameters: Seq[Parameter], tpe: ParameterType, head: Seq[AlgorithmHeadPart], body: Seq[Statement]) extends Declaration

case class LocalDeclaration(variableNames: Seq[String], parameters: ParameterType, defaultExpression: Option[Expression]) extends AlgorithmHeadPart


case class ProcedureDeclaration(name: String, parameters: Option[ProcedureParameters], head: Seq[AlgorithmHeadPart], body: Seq[Statement]) extends Declaration

case class ReferenceClause(schemaRef: String, imports: Seq[RenamedResource]) extends InterfaceSpecification
case class RuleDeclaration(ruleId: String, entityRefs: Seq[String], head: Seq[AlgorithmHeadPart], body: Seq[Statement], whereClause: WhereClause) extends SchemaBody
case class Schema(id: String, version: Option[String], body: Seq[SchemaBody])
case class SubtypeConstraintDeclaration(name: String, entity: String, abstractSuperType:Boolean, totalOver: Seq[String], of: Option[SupertypeExpression]) extends Declaration 
case class TypeDeclaration(name: String, underlyingType: UnderlyingType, whereClause: Option[WhereClause]) extends Declaration
case class UseClause(schemaRef: String, imports: Seq[RenamedType]) extends InterfaceSpecification