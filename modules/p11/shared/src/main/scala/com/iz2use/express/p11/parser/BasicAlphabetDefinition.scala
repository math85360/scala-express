package com.iz2use.express.p11.parser

import scala.language.postfixOps
import com.iz2use.express.p11.ast
import fastparse.all._

object BasicAlphabetDefinition {
  private[parser] val bit = P(CharIn("01"))
  private val digit = P(CharIn('0' to '9'))
  private[parser] val digits = P(digit.rep(1))
  private[parser] val encoded_character = P(octet ~ octet ~ octet ~ octet)
  private val hex_digit = P(digit | CharIn('a' to 'f'))
  private val letter = P(CharIn('a' to 'z', 'A' to 'Z'))
  private val lparen_then_not_lparen_star = P("(".rep(1) ~ not_lparen_star.rep(1))
  private val not_lparen_star = P(not_paren_star | ")")
  private val not_paren_star = P(letter | digit | not_paren_star_special)
  private val not_paren_star_quote_special = P(CharIn("!\"#$%&+,-./:;<=>?@[\\]^_`{|}~"))
  private val not_paren_star_special = P(not_paren_star_quote_special | "'")
  private[parser] val not_quote = P(not_paren_star_quote_special | letter | digit | "(" | ")" | "*")
  private val not_rparen_star = P(not_paren_star | "(")
  private val octet = P(hex_digit ~ hex_digit)
  private val special = P(not_paren_star_quote_special | "(" | ")" | "*" | "'")
  private val not_rparen_star_then_rparen = P(not_rparen_star.rep(1) ~ ")".rep(1))

  private[parser] val simple_id_tail = P((letter | digit | "_"))
  private val simple_id = P((letter ~ simple_id_tail.rep).!)
  private[parser] val sign = P("+" | "-")

  private[parser] val embedded_remark: P[String] = P(("(*" ~ remark_tag.? ~ (not_paren_star.rep(1) | lparen_then_not_lparen_star | "*".rep(1) | not_rparen_star_then_rparen | embedded_remark).rep ~ "*)").!)
  private val remark = P(embedded_remark | tail_remark)
  private val remark_tag = P("\"" ~ remark_ref ~ ("." ~ remark_ref).rep(1) ~ "\"")

  private[parser] val attribute_id = P(simple_id.map(ast.RefOrId[ast.AttributeId]))
  private[parser] val constant_id = P(simple_id.map(ast.RefOrId[ast.ConstantId]))
  private[parser] val entity_id = P(simple_id.map(ast.RefOrId[ast.EntityId]))
  private[parser] val enumeration_id = P(simple_id.map(ast.RefOrId[ast.EnumerationId]))
  private[parser] val function_id = P(simple_id.map(ast.RefOrId[ast.FunctionId]))
  private[parser] val parameter_id = P(simple_id.map(ast.RefOrId[ast.ParameterId]))
  private[parser] val procedure_id = P(simple_id.map(ast.RefOrId[ast.ProcedureId]))
  private[parser] val rule_id = P(simple_id.map(ast.RefOrId[ast.RuleId]))
  private[parser] val rule_label_id = P(simple_id.map(ast.RefOrId[ast.RuleLabelId]))
  private[parser] val schema_id = P(simple_id.map(ast.RefOrId[ast.SchemaId]))
  private[parser] val subtype_constraint_id = P(simple_id.map(ast.RefOrId[ast.SubtypeConstraintId]))
  private[parser] val type_id = P(simple_id.map(ast.RefOrId[ast.TypeId]))
  private[parser] val type_label_id = P(simple_id.map(ast.RefOrId[ast.TypeLabelId]))
  private[parser] val variable_id = P(simple_id.map(ast.RefOrId[ast.VariableId]))

  private[parser] val attribute_ref = P(attribute_id.map(ast.RefOrId[ast.AttributeRef](_)))
  private[parser] val constant_ref = P(constant_id.map(ast.RefOrId[ast.ConstantRef]))
  private[parser] val entity_ref = P(entity_id.map(ast.RefOrId[ast.EntityRef]))
  private[parser] val enumeration_ref = P(enumeration_id.map(ast.RefOrId[ast.EnumerationRef]))
  private[parser] val function_ref = P(function_id.map(ast.RefOrId[ast.FunctionRef]))
  private[parser] val general_ref = P(parameter_ref | variable_ref)
  private[parser] val parameter_ref = P(parameter_id.map(ast.RefOrId[ast.ParameterRef]))
  private[parser] val procedure_ref = P(procedure_id.map(ast.RefOrId[ast.ProcedureRef]))
  private[parser] val rule_label_ref = P(rule_label_id.map(ast.RefOrId[ast.RuleLabelRef]))
  private[parser] val rule_ref = P(rule_id.map(ast.RefOrId[ast.RuleRef]))
  private[parser] val schema_ref = P(schema_id.map(ast.RefOrId[ast.SchemaRef]))
  private[parser] val subtype_constraint_ref = P(subtype_constraint_id.map(ast.RefOrId[ast.SubtypeConstraintRef]))
  private[parser] val type_label_ref = P(type_label_id.map(ast.RefOrId[ast.TypeLabelRef]))
  private[parser] val type_ref = P(type_id.map(ast.RefOrId[ast.TypeRef]))
  private[parser] val variable_ref = P(variable_id.map(ast.RefOrId[ast.VariableRef]))

  private[parser] val remark_ref = P(attribute_ref | constant_ref | entity_ref | enumeration_ref | function_ref | parameter_ref | procedure_ref | rule_label_ref | rule_ref | schema_ref | subtype_constraint_ref | type_label_ref | type_ref | variable_ref)
  private[parser] val tail_remark = P("--" ~ remark_tag.? ~ ("\u0007" | " " | "\b" | "\t" | "\n" | "\u000b" | "\f" | "\r").rep ~ "\n")
  private[parser] val space = P(CharsWhileIn(" \t\n\f\r", 1))
  private[parser] val comment = P("(*" ~/ (!"*)" ~ AnyChar).rep ~ "*)" ~/)
  private[parser] val spaceOrComments : P[Unit]= P((space | comment).rep(1))
  private[parser] val spaceOrCommentsOpt: P[Unit] = P(spaceOrComments.?.map(_ => ()))

}