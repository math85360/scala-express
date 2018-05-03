package com.iz2use.express.parser

import com.iz2use.express.ast
import scala.language.postfixOps
import IgnoringParts._
import fastparse.noApi._

trait TokenDefinition extends Types {
  private val abstract_entity_declaration: P[Unit] = P(ABSTRACT)
  private val abstract_supertype: P[Unit] = P(ABSTRACT ~ SUPERTYPE ~ ";")
  private val abstract_supertype_declaration: P[Unit] = P(ABSTRACT ~ SUPERTYPE ~ subtype_constraint.?)

  private val algorithm_head: P[Seq[ast.AlgorithmHeadPart]] = P(declaration.rep ~ constant_decl.? ~ local_decl.?)
  private val alias_stmt: P[Unit] = P(ALIAS ~ variable_id ~ FOR ~ general_ref ~ qualifier.rep ~ ";" ~ stmt.rep(1) ~ END_ALIAS ~ ";")
  private val assignment_stmt: P[Unit] = P(general_ref ~ qualifier.rep ~ "::P[Unit]=" ~ expression ~ ";")
  private val attribute_decl: P[Unit] = P(attribute_id | redeclared_attribute)

  private val built_in_procedure: P[Unit] = P(INSERT | REMOVE)
  private val case_action: P[Unit] = P(case_label.nonEmptyList ~ ":" ~ stmt)
  private val case_label = P(expression)
  private val case_stmt: P[Unit] = P(CASE ~ selector ~ OF ~ case_action.rep ~ (OTHERWISE ~ ":" ~ stmt).? ~ END_CASE ~ ";")
  private val compound_stmt: P[Unit] = P(BEGIN ~ stmt.rep ~ END ~ ";")
  private val constant_body: P[ast.ConstantDeclaration] = P(constant_id ~ ":" ~ instantiable_type ~ "=" ~ expression ~ ";")
    .map(ast.ConstantDeclaration.tupled)
  private val constant_decl: P[Seq[ast.ConstantDeclaration]] = P(CONSTANT ~ constant_body.rep(1) ~ END_CONSTANT ~ ";")

  private val constructed_types: P[Unit] = P(enumeration_type | select_type)

  private val declaration: P[ast.Declaration] = P(entity_decl | function_decl | procedure_decl | subtype_constraint_decl | type_decl)
  private val derived_attr: P[Unit] = P(attribute_decl ~ ":" ~ parameter_type ~ ":=" ~ expression ~ ";")
  private val derive_clause: P[Unit] = P(DERIVE ~ derived_attr.rep(1))
  private val domain_rule: P[ast.DomainRule] = P(((rule_label_id ~ ":").? ~ expression)
    .map(ast.DomainRule.tupled))

  private val entity_body: P[Unit] = P(explicit_attr.rep ~ derive_clause.? ~ inverse_clause.? ~ unique_clause.? ~ where_clause.?)

  private val entity_decl: P[ast.EntityDeclaration] = P(entity_head ~ entity_body ~ END_ENTITY ~ ";")
  private val entity_head: P[Unit] = P(ENTITY ~ entity_id ~ subsuper ~ ";")

  private val enumeration_extension: P[Unit] = P(BASED_ON ~ type_ref ~ (WITH ~ enumeration_items).?)

  private val enumeration_items: P[Unit] = P("(" ~ enumeration_id.nonEmptyList ~ ")")

  private val enumeration_type: P[Unit] = P(EXTENSIBLE.? ~ ENUMERATION ~ ((OF ~ enumeration_items) | enumeration_extension).?)
  private val escape_stmt: P[Unit] = P(ESCAPE ~ ";")
  private val explicit_attr: P[Unit] = P(attribute_decl.nonEmptyList ~ ":" ~ OPTIONAL.? ~ parameter_type ~ ";")

  private val formal_parameter: P[Unit] = P(parameter_id.nonEmptyList ~ ":" ~ parameter_type)

  private val function_decl: P[Unit] = P(function_head ~ algorithm_head ~ stmt.rep(1) ~ END_FUNCTION ~ ";")
  private val function_head: P[Unit] = P(FUNCTION ~ function_id ~ ("(" ~ formal_parameter.rep(1, ";") ~ ")").? ~ ":" ~ parameter_type ~ ";")

  private val if_stmt: P[Unit] = P(IF ~ logical_expression ~ THEN ~ stmt.rep(1) ~ (ELSE ~ stmt.rep(1)).? ~ END_IF ~ ";")
  private val increment: P[Unit] = P(numeric_expression)
  private val increment_control: P[Unit] = P(variable_id ~ "::P[Unit]=" ~ bound_1 ~ TO ~ bound_2 ~ (BY ~ increment).?)

  private val interface_specification: P[ast.InterfaceSpecification] = P(reference_clause | use_clause)

  private val inverse_attr: P[Unit] = P(attribute_decl ~ ":" ~ ((SET | BAG) ~ bound_spec.? ~ OF).? ~ entity_ref ~ FOR ~ (entity_ref ~ ".").? ~ attribute_ref ~ ";")
  private val inverse_clause: P[Unit] = P(INVERSE ~ inverse_attr.rep(1))

  private val local_decl: P[Seq[ast.LocalDeclaration]] = P(LOCAL ~ local_variable.rep(1) ~ END_LOCAL ~ ";")

  private val local_variable: P[ast.LocalDeclaration] = P((variable_id.nonEmptyList ~ ":" ~ parameter_type ~ (":=" ~ expression).? ~ ";")
    .map(ast.LocalDeclaration.tupled))

  private val null_stmt: P[Unit] = P(";")

  private val one_of: P[Unit] = P(ONEOF ~ "(" ~ supertype_expression.nonEmptyList ~ ")")

  private val procedure_call_stmt: P[Unit] = P((built_in_procedure | procedure_ref) ~ actual_parameter_list.? ~ ";")
  private val procedure_decl: P[Unit] = P(procedure_head ~ algorithm_head ~ stmt.rep ~ END_PROCEDURE ~ ";")
  private val procedure_head: P[Unit] = P(PROCEDURE ~ procedure_id ~ ("(" ~ VAR.? ~ formal_parameter.rep(1, ";") ~ ")").? ~ ";")

  private val qualified_attribute: P[Unit] = P(SELF ~ group_qualifier ~ attribute_qualifier)

  private val redeclared_attribute: P[Unit] = P(qualified_attribute ~ (RENAMED ~ attribute_id).?)
  private val referenced_attribute: P[Unit] = P(attribute_ref | qualified_attribute)
  private val reference_clause: P[ast.ReferenceClause] = P((REFERENCE ~ FROM ~ schema_ref ~ ("(" ~ resource_or_rename.nonEmptyList ~ ")").? ~ ";")
    .map(ast.ReferenceClause.tupled))

  private val rename_id: P[Unit] = P(constant_id | entity_id | function_id | procedure_id | type_id)
  private val repeat_control: P[Unit] = P(increment_control.? ~ while_control.? ~ until_control.?)
  private val repeat_stmt: P[Unit] = P(REPEAT ~ repeat_control ~ ";" ~ (stmt).rep(1) ~ END_REPEAT ~ ";")

  private val resource_or_rename: P[ast.ResourceOrRename] = P((resource_ref ~ (AS ~ rename_id).?).map({
      case (resourceRef, None)     => resourceRef
      case (resourceRef, Some(as)) => ast.RenamedResource(resourceRef, as)
    }))
  private val resource_ref : P[Ref] = P(constant_ref | entity_ref | function_ref | procedure_ref | type_ref)
  private val return_stmt: P[Unit] = P(RETURN ~ ("(" ~ expression ~ ")").? ~ ";")
  private val rule_decl: P[ast.Rule] = P((rule_head ~ algorithm_head ~ stmt.rep ~ where_clause ~ END_RULE ~ ";")
    .map(ast.Rule.tupled))
  private val rule_head = P(RULE ~ rule_id ~ FOR ~ "(" ~ entity_ref.nonEmptyList ~ ")" ~ ";")

  private val schema_body: P[Seq[ast.SchemaBody]] = P(interface_specification.rep ~ constant_decl.? ~ (declaration | rule_decl).rep)
  private val schema_decl: P[ast.Schema] = P(SCHEMA ~ schema_id ~ schema_version_id.? ~ ";" ~ schema_body ~ END_SCHEMA ~ ";")
    .map(ast.Schema.tupled)

  private val schema_version_id = P(string_literal)
  private val selector: P[Unit] = P(expression)
  private val select_extension: P[Unit] = P(BASED_ON ~ type_ref ~ (WITH ~ select_list).?)
  private val select_list: P[Unit] = P("(" ~ named_types.nonEmptyList ~ ")")
  private val select_type: P[Unit] = P((EXTENSIBLE ~ GENERIC_ENTITY.?).? ~ SELECT ~ (select_list | select_extension).?)

  private val skip_stmt: P[Unit] = P(SKIP ~ ";")
  private val stmt: P[ast.Statement] = P(alias_stmt | assignment_stmt | case_stmt | compound_stmt | escape_stmt | if_stmt | null_stmt | procedure_call_stmt | repeat_stmt | return_stmt | skip_stmt)

  private val subsuper: P[Unit] = P(supertype_constraint.? ~ subtype_declaration.?)
  private val subtype_constraint: P[Unit] = P(OF ~ "(" ~ supertype_expression ~ ")")
  private val subtype_constraint_body: P[Unit] = P(abstract_supertype.? ~ total_over.? ~ (supertype_expression ~ ";").?)
  private val subtype_constraint_decl: P[Unit] = P(subtype_constraint_head ~ subtype_constraint_body ~ END_SUBTYPE_CONSTRAINT ~ ";")
  private val subtype_constraint_head: P[Unit] = P(SUBTYPE_CONSTRAINT ~ subtype_constraint_id ~ FOR ~ entity_ref ~ ";")

  private val subtype_declaration: P[Unit] = P(SUBTYPE ~ OF ~ "(" ~ entity_ref.nonEmptyList ~ ")")
  private val supertype_constraint: P[Unit] = P(abstract_entity_declaration | abstract_supertype_declaration | supertype_rule)
  private val supertype_expression: P[Unit] = P(supertype_factor ~ (ANDOR ~ supertype_factor).rep)
  private val supertype_factor: P[Unit] = P(supertype_term ~ (AND ~ supertype_term).rep)
  private val supertype_rule: P[Unit] = P(SUPERTYPE ~ subtype_constraint)
  private val supertype_term: P[Unit] = P(entity_ref | one_of | "(" ~ supertype_expression ~ ")")
  val syntax: P[Seq[ast.Schema]] = P(schema_decl.rep(1))

  private val total_over: P[Unit] = P(TOTAL_OVER ~ "(" ~ entity_ref.nonEmptyList ~ ")" ~ ";")
  private val type_decl: P[Unit] = P(TYPE ~ type_id ~ "=" ~ underlying_type ~ ";" ~ where_clause.? ~ END_TYPE ~ ";")

  private val underlying_type: P[Unit] = P(concrete_types | constructed_types)
  private val unique_clause: P[Unit] = P(UNIQUE ~ unique_rule ~ ";" ~ (unique_rule ~ ";").rep)
  private val unique_rule: P[Unit] = P((rule_label_id ~ ":").? ~ referenced_attribute.nonEmptyList)
  private val until_control: P[Unit] = P(UNTIL ~ logical_expression)
  private val use_clause: P[ast.UseClause] = P((USE ~ FROM ~ schema_ref ~ ("(" ~ named_type_or_rename.nonEmptyList ~ ")").? ~ ";")
    .map(ast.UseClause.tupled))

  private val where_clause: P[ast.WhereClause] = P((WHERE ~ domain_rule.rep(1, ";"))
    .map(ast.WhereClause))
  private val while_control: P[Unit] = P(WHILE ~ logical_expression)
}