package com.iz2use.express.parser

import com.iz2use.express.ast
import scala.language.postfixOps
import IgnoringParts._
import fastparse.noApi._
import shapeless.{ :+:, CNil }

trait ExpressFileStructure {
  import Types._
  private val abstract_entity_declaration: P[ast.AbstractEntityDeclaration.type] = P((ABSTRACT)
    .to(ast.AbstractEntityDeclaration))
  private val abstract_supertype = P(ABSTRACT ~ SUPERTYPE ~ ";")
  private val abstract_supertype_declaration: P[ast.AbstractSupertypeDeclaration] = P((ABSTRACT ~ SUPERTYPE ~ subtype_constraint.?)
    .map(ast.AbstractSupertypeDeclaration))

  private val algorithm_head: P[Seq[ast.AlgorithmHeadPart]] = P((declaration.rep ~ constant_decl.? ~ local_decl.?)
    .map { case (a, b, c) => a ++ b ++ c })
  private val alias_stmt: P[ast.AliasStatement] = P((ALIAS ~ variable_id.map(_.name) ~ FOR ~ general_ref.map(_.name) ~ qualifier.rep ~ ";" ~ stmt.rep(1) ~ END_ALIAS ~ ";")
    .map(ast.AliasStatement.tupled))
  private val assignment_stmt: P[ast.AssignmentStatement] = P((general_ref.map(_.name) ~ qualifier.rep ~ ":=" ~ expression ~ ";")
    .map(ast.AssignmentStatement.tupled))
  private val attribute_decl = P(attribute_id.map(r => ast.SimpleAttributeName(r.name)) | redeclared_attribute)

  private val built_in_procedure: P[ast.BuiltInProcedure] = P(INSERT.to(ast.BuiltInProcedure.Insert) | REMOVE.to(ast.BuiltInProcedure.Remove))
  private val case_action = P((case_label.nonEmptyList ~ ":" ~ stmt)
    .map(ast.Case.tupled))
  private val case_label = P(expression)
  private val case_stmt: P[ast.CaseStatement] = P((CASE ~ selector ~ OF ~ case_action.rep ~ (OTHERWISE ~ ":" ~ stmt).? ~ END_CASE ~ ";")
    .map(ast.CaseStatement.tupled))
  private val compound_stmt: P[ast.CompoundStatement] = P((BEGIN ~ stmt.rep ~ END ~ ";")
    .map(ast.CompoundStatement))
  private val constant_body: P[ast.ConstantDeclaration] = P((constant_id.map(_.name) ~ ":" ~ instantiable_type ~ "=" ~ expression ~ ";")
    .map(ast.ConstantDeclaration.tupled))
  private val constant_decl: P[Seq[ast.ConstantDeclaration]] = P(CONSTANT ~ constant_body.rep(1) ~ END_CONSTANT ~ ";")

  private val constructed_types: P[ast.ConstructedType] = P(enumeration_type | select_type)

  private val declaration: P[ast.Declaration] = P(entity_decl | function_decl | procedure_decl | subtype_constraint_decl | type_decl)
  private val derived_attr: P[ast.DerivedAttribute] = P((attribute_decl ~ ":" ~ parameter_type ~ ":=" ~ expression ~ ";")
    .map(ast.DerivedAttribute.tupled))
  private val derive_clause = P(DERIVE ~ (!(INVERSE | UNIQUE | WHERE| END) ~ derived_attr).rep(1))

  private val entity_body = P(explicit_attr_list ~ derive_clause.? ~ inverse_clause.? ~ unique_clause.? ~ where_clause.?)

  private[parser] val entity_decl: P[ast.EntityDeclaration] = P((entity_head ~ entity_body ~ END_ENTITY ~ ";")
    .map { case (a, (b, c), (d, e, f, g, h)) => ast.EntityDeclaration(a, b, c, d, e, f, g, h) })
  private val entity_head = P(ENTITY ~ entity_id.map(_.name) ~ subsuper ~ ";")

  private val enumeration_extension: P[ast.BasedOnEnumeration] = P((BASED_ON ~ type_ref.map(_.name) ~ (WITH ~ enumeration_items).?)
    .map(ast.BasedOnEnumeration.tupled))

  private val enumeration_items: P[Seq[ast.EnumerationItem]] = P("(" ~ enumeration_id.map(r => ast.EnumerationItem(r.name)).nonEmptyList ~ ")")

  private val enumeration_type: P[ast.EnumerationType] = P((EXTENSIBLE.? ~ ENUMERATION ~ ((OF ~ enumeration_items).map(Left(_)) | enumeration_extension.map(Right(_))).?)
    .map(ast.EnumerationType.tupled))
  private val escape_stmt = P(ESCAPE.to(ast.EscapeStatement) ~ ";")
  private val explicit_attr = P(attribute_decl.nonEmptyList ~ ":" ~ OPTIONAL.? ~ parameter_type ~ ";")

  private val explicit_attr_list: P[Seq[ast.ExplicitAttribute]] = P((!(DERIVE | INVERSE | UNIQUE | WHERE| END) ~ explicit_attr).rep.map(_.flatMap {
    case (names, opt, tpe) => names.map(name => ast.ExplicitAttribute(name, opt, tpe))
  }))

  private val formal_parameter: P[Seq[ast.Parameter]] = P((parameter_id.nonEmptyList ~ ":" ~ parameter_type)
    .map { case (names, tpe) => names.map(name => ast.Parameter(name.name, tpe)) })

  private val function_decl: P[ast.FunctionDeclaration] = P((function_head ~ algorithm_head ~ stmt.rep(1) ~ END_FUNCTION ~ ";")
    .map(ast.FunctionDeclaration.tupled))
  private val function_head = P(FUNCTION ~ function_id.map(_.name) ~ ("(" ~ formal_parameter.rep(1, ";").map(_.flatten) ~ ")").? ~ ":" ~ parameter_type ~ ";")

  private val if_stmt: P[ast.IfStatement] = P((IF ~ logical_expression ~ THEN ~ stmt.rep(1) ~ (ELSE ~ stmt.rep(1)).? ~ END_IF ~ ";")
    .map(ast.IfStatement.tupled))
  private val increment = P(numeric_expression)
  private val increment_control = P((variable_id.map(_.name) ~ ":=" ~ bound_1 ~ TO ~ bound_2 ~ (BY ~ increment).?)
    .map(ast.IncrementControl.tupled))

  private val interface_specification: P[ast.InterfaceSpecification] = P(reference_clause | use_clause)

  private val inverse_attr: P[ast.InverseAttribute] = P((attribute_decl ~ ":" ~ ((SET.to(true) | BAG.to(false)) ~ bound_spec.? ~ OF).map(ast.InverseAggregateType.tupled).? ~ entity_ref.map(_.name) ~ FOR ~ (entity_ref.map(_.name) ~ ".").? ~ attribute_ref.map(_.name) ~ ";")
    .map(ast.InverseAttribute.tupled))
  private val inverse_clause = P(INVERSE ~ (!(UNIQUE | WHERE| END) ~ inverse_attr).rep(1))

  private val local_decl: P[Seq[ast.LocalDeclaration]] = P(LOCAL ~ local_variable.rep(1) ~ END_LOCAL ~ ";")

  private val local_variable: P[ast.LocalDeclaration] = P((variable_id.map(_.name).nonEmptyList ~ ":" ~ parameter_type ~ (":=" ~ expression).? ~ ";")
    .map(ast.LocalDeclaration.tupled))

  private val null_stmt: P[ast.NullStatement.type] = P(";".to(ast.NullStatement))

  private val one_of: P[ast.SupertypeOneOf] = P((ONEOF ~ "(" ~ supertype_expression.nonEmptyList ~ ")")
    .map(ast.SupertypeOneOf))

  private val procedure_call_stmt: P[ast.ProcedureCallStatement] = P(((built_in_procedure | procedure_ref.map(r => ast.UserDefinedProcedure(r.name))) ~ actual_parameter_list.? ~ ";")
    .map(ast.ProcedureCallStatement.tupled))
  private val procedure_decl: P[ast.ProcedureDeclaration] = P((procedure_head ~ algorithm_head ~ stmt.rep ~ END_PROCEDURE ~ ";")
    .map(ast.ProcedureDeclaration.tupled))
  private val procedure_head = P(PROCEDURE ~ procedure_id.map(_.name) ~ ("(" ~ VAR.? ~ formal_parameter.rep(1, ";").map(_.flatten) ~ ")").map(ast.ProcedureParameters.tupled).? ~ ";")

  private val qualified_attribute = P((SELF ~ group_qualifier.map(_.name) ~ attribute_qualifier.map(_.name))
    .map(ast.QualifiedAttribute.tupled))

  private val redeclared_attribute: P[ast.RedeclaredAttribute] = P((qualified_attribute ~ (RENAMED ~ attribute_id.map(_.name)).?)
    .map(ast.RedeclaredAttribute.tupled))
  private val referenced_attribute: P[ast.UniqueSource] = P(attribute_ref.map(r => ast.ReferencedAttribute(r.name)) | qualified_attribute)
  private val reference_clause: P[ast.ReferenceClause] = P((REFERENCE ~ FROM ~ schema_ref.map(_.name) ~ ("(" ~ resource_or_rename.nonEmptyList ~ ")").? ~ ";")
    .map(ast.ReferenceClause.tupled))

  private val rename_id =
    P(constant_id | entity_id | function_id | procedure_id | type_id)
  private val repeat_control = P(increment_control.? ~ while_control.? ~ until_control.?)
  private val repeat_stmt: P[ast.RepeatStatement] = P((REPEAT ~ repeat_control ~ ";" ~ (stmt).rep(1) ~ END_REPEAT ~ ";")
    .map(ast.RepeatStatement.tupled))

  private val resource_or_rename: P[ast.RenamedResource] = P((resource_ref.map(_.name) ~ (AS ~ rename_id.map(_.name)).?).map({
    case (resourceRef, as) => ast.RenamedResource(resourceRef, as.getOrElse(resourceRef))
  }))
  private val resource_ref =
    P(constant_ref | entity_ref | function_ref | procedure_ref | type_ref)
  private val return_stmt: P[ast.ReturnStatement] = P((RETURN ~ ("(" ~ expression ~ ")").? ~ ";")
    .map(ast.ReturnStatement))
  private val rule_decl: P[ast.RuleDeclaration] = P((rule_head ~ algorithm_head ~ stmt.rep ~ where_clause ~ END_RULE ~ ";")
    .map(ast.RuleDeclaration.tupled))
  private val rule_head = P(RULE ~ rule_id.map(_.name) ~ FOR ~ "(" ~ entity_ref.map(_.name).nonEmptyList ~ ")" ~ ";")

  private val schema_body: P[Seq[ast.SchemaBody]] = P((interface_specification.rep ~ constant_decl.? ~ (declaration | rule_decl).rep)
    .map { case (a, b, c) => a ++ b ++ c })
  private val schema_decl: P[ast.Schema] = P((SCHEMA ~ schema_id.map(_.name) ~ schema_version_id.map(_.value).? ~ ";" ~ schema_body ~ END_SCHEMA ~ ";")
    .map(ast.Schema.tupled))

  private val schema_version_id = P(string_literal)
  private val selector = P(expression)
  private val select_extension = P((BASED_ON ~ type_ref.map(_.name) ~ (WITH ~ select_list).?)
    .map(ast.SelectTypeFromBasedOn.tupled))
  private val select_list = P("(" ~ named_types.map(_.name).nonEmptyList ~ ")")
  private val select_type: P[ast.SelectType] = P(((EXTENSIBLE ~ GENERIC_ENTITY.?).map(ast.SelectTypeExtensible).? ~ SELECT ~ (select_list.map(Left(_)) | select_extension.map(Right(_))).?)
    .map(ast.SelectType.tupled))

  private val skip_stmt: P[ast.SkipStatement.type] = P(SKIP.to(ast.SkipStatement) ~ ";")
  private val stmt: P[ast.Statement] = P(
    alias_stmt |
      assignment_stmt |
      case_stmt |
      compound_stmt |
      escape_stmt |
      if_stmt |
      null_stmt |
      procedure_call_stmt |
      repeat_stmt |
      return_stmt |
      skip_stmt)

  private val subsuper = P(supertype_constraint.? ~ subtype_declaration.?)
  private val subtype_constraint: P[ast.SupertypeExpression] = P(OF ~ "(" ~ supertype_expression ~ ")")
  private val subtype_constraint_body = P(abstract_supertype.? ~ total_over.? ~ (supertype_expression ~ ";").?)
  private val subtype_constraint_decl: P[ast.SubtypeConstraintDeclaration] = P((subtype_constraint_head ~ subtype_constraint_body ~ END_SUBTYPE_CONSTRAINT ~ ";")
    .map { case (a, b, (c, d, e)) => ast.SubtypeConstraintDeclaration(a, b, c, d, e) })
  private val subtype_constraint_head = P(SUBTYPE_CONSTRAINT ~ subtype_constraint_id.map(_.name) ~ FOR ~ entity_ref.map(_.name) ~ ";")

  private val subtype_declaration: P[ast.SubtypeConstraint] = P((SUBTYPE ~ OF ~ "(" ~ entity_ref.map(_.name).nonEmptyList ~ ")")
    .map(ast.SubtypeConstraint))
  private val supertype_constraint: P[ast.SupertypeConstraint] = P(abstract_supertype_declaration | abstract_entity_declaration | supertype_rule)
  private val supertype_expression: P[ast.SupertypeExpression] = P(supertype_factor.rep(1, ANDOR).map {
    case Seq(a) => a
    case lst    => ast.SupertypeProduct(lst)
  })
  private val supertype_factor: P[ast.SupertypeExpression] = P(supertype_term.rep(1, !ANDOR ~ AND).map {
    case Seq(a) => a
    case lst    => ast.SupertypeSum(lst)
  })
  private val supertype_rule: P[ast.SupertypeRule] = P((SUPERTYPE ~ subtype_constraint)
    .map(ast.SupertypeRule))
  private val supertype_term: P[ast.SupertypeExpression] = P(entity_ref.map(r => ast.UserDefinedEntity(r.name)) | one_of | "(" ~ supertype_expression ~ ")")
  private val syntax: P[Seq[ast.Schema]] = P(schema_decl.rep(1))

  private val total_over = P(TOTAL_OVER ~ "(" ~ entity_ref.map(_.name).nonEmptyList ~ ")" ~ ";")
  protected[parser] val type_decl: P[ast.TypeDeclaration] = P((TYPE ~/ type_id.map(_.name) ~ "=" ~/ underlying_type ~ ";" ~/ where_clause.? ~ END_TYPE ~ ";" ~/)
    .map(ast.TypeDeclaration.tupled))

  private val underlying_type: P[ast.UnderlyingType] = P(constructed_types | concrete_types)
  private val unique_clause = P(UNIQUE ~ (!(WHERE | END) ~ unique_rule).rep(1, ";"))
  private val unique_rule: P[ast.UniqueClause] = P(((rule_label_id.map(_.name) ~ ":").? ~ referenced_attribute.nonEmptyList)
    .map(ast.UniqueClause.tupled))
  private val until_control = P(UNTIL ~ logical_expression)
  private val use_clause: P[ast.UseClause] = P((USE ~ FROM ~ schema_ref.map(_.name) ~ ("(" ~ named_type_or_rename.nonEmptyList ~ ")").? ~ ";")
    .map(ast.UseClause.tupled))

  private val where_clause: P[ast.WhereClause] = P((WHERE ~/ domain_rule.rep(1, ";" ~ !(END)) ~ ";")
    .map(ast.WhereClause))

  private val while_control = P(WHILE ~ logical_expression)

  val root = P(Pass ~ syntax ~ tail_remark.?.map(_ => ()) ~ End)
}