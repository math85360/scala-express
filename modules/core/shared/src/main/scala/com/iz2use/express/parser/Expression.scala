package com.iz2use.express.parser

import com.iz2use.express.ast
import scala.language.postfixOps
import IgnoringParts._
import fastparse.noApi._

trait Expression extends Literal {
  private[parser] val actual_parameter_list: P[Seq[ast.Expression]] = P(("(" ~ parameter.nonEmptyList ~ ")"))
  private val add_like_op: P[ast.AddLikeOp] = P(
    "+".to(ast.AdditionOp) | "-".to(ast.SubtractionOp) |
      OR.to(ast.OrOp) | XOR.to(ast.XorOp))
  private val aggregate_initializer: P[ast.AggregateInitializer] = P(("[" ~ element.nonEmptyList ~ "]")
      .map(ast.AggregateInitializer.apply))
  private val aggregate_source = P(simple_expression)
  private[parser] val attribute_qualifier: P[ast.AttributeQualifier] = P("." ~ attribute_ref)
    .map(ast.AttributeQualifier)
  private[parser] val bound_1 = P(numeric_expression)
  private[parser] val bound_2 = P(numeric_expression)
  private[parser] val bound_spec = P("[" ~ bound_1 ~ ":" ~ bound_2 ~ "]")
    .map(ast.Bounds.tupled.apply)
  private val built_in_constant = P((CONST_E | PI | SELF | "?").!
    .map(ast.BuiltInConstant.apply))
  private val built_in_function: P[ast.BuiltInFunction] = P((ABS | ACOS | ASIN | ATAN | BLENGTH | COS | EXISTS | EXP | FORMAT | HIBOUND | HIINDEX | LENGTH | LOBOUND | LOINDEX | LOG | LOG2 | LOG10 | NVL | ODD | ROLESOF | SIN | SIZEOF | SQRT | TAN | TYPEOF | USEDIN | VALUE | VALUE_IN | VALUE_UNIQUE).!
    .map(ast.BuiltInFunction.apply))
  private val constant_factor: P[ast.ConstantFactor] = P(built_in_constant | constant_ref)
  private val element: P[ast.Expression] = P(expression ~ (":".to(ast.RepetitionOp) ~ repetition).?)
  private val entity_constructor: P[ast.EntityConstructor] = P((entity_ref ~ "(" ~ expression.nonEmptyList.? ~ ")")
    .map(ast.EntityConstructor.tupled.apply))
  private val enumeration_reference: P[ast.EnumerationReference] = P(((type_ref ~ ".").? ~ enumeration_ref)
    .map(ast.EnumerationReference.tupled.apply))
  private[parser] val expression: P[ast.Expression] = P(simple_expression ~ (rel_op_extended ~ simple_expression).?)
  private val factor: P[ast.Expression] = P(simple_factor ~ ("**".to(ast.PowerOp) ~ simple_factor).?)
  private val function_call: P[ast.FunctionCall] = P((built_in_function | function_ref) ~ actual_parameter_list.?)
    .map(ast.FunctionCall.tupled.apply)
  private[parser] val group_qualifier: P[ast.GroupQualifier] = P("\\" ~ entity_ref)
    .map(ast.GroupQualifier.apply)
  private val index_qualifier: P[ast.IndexQualifier] = P("[" ~ index_1 ~ (":" ~ index_2).? ~ "]")
    .map(ast.IndexQualifier.tupled.apply)
  private val index = P(numeric_expression)
  private val index_1 = P(index)
  private val index_2 = P(index)
  private val interval: P[ast.Interval] = P("{" ~ interval_low ~ interval_op ~ interval_item ~ interval_op ~ interval_high ~ "}")
    .map(ast.Interval.tupled.apply)
  private val interval_high = P(simple_expression)
  private val interval_item = P(simple_expression)
  private val interval_low = P(simple_expression)
  private val interval_op: P[Boolean] = P("<" ~ "=".?)
  private[parser] val logical_expression: P[ast.LogicalExpression] = P(expression)
  private val multiplication_like_op: P[ast.MultiplicationLikeOp] = P(
    "*".to(ast.MultiplicationOp) | "/".to(ast.DivisionOp) |
      DIV.to(ast.DivisionOp) | MOD.to(ast.ModuloOp) |
      AND.to(ast.AndOp) | "||".to(ast.OrElseOp))
  private[parser] val numeric_expression: P[ast.NumericExpression] = P(simple_expression)
  private val parameter = P(expression)
  private val population = P(entity_ref)
  private val primary: P[ast.Primary] = P(literal | (qualifiable_factor ~ qualifier.rep).map(ast.QualifiedApply.tupled))
  private val qualifiable_factor: P[ast.QualifiableFactor] = P(attribute_ref | constant_factor | function_call | general_ref | population)
  private[parser] val qualifier: P[ast.Qualifier] = P(attribute_qualifier | group_qualifier | index_qualifier)
  private val query_expression: P[ast.QueryExpression] = P(QUERY ~ "(" ~ variable_id ~ "<*" ~ aggregate_source ~ "|" ~ logical_expression ~ ")")
    .map(ast.QueryExpression.tupled.apply)
  private val rel_op = P(
    "<".to(ast.LT) | ">".to(ast.GT) |
      "<=".to(ast.LE) | ">=".to(ast.GE) |
      "<>".to(ast.NE) | "=".to(ast.EQ) |
      ":<>:".to(ast.TN) | ":=:".to(ast.TE))
  private val rel_op_extended: P[ast.RelOpExtended] = P(rel_op | IN.to(ast.IN) | LIKE.to(ast.LIKE))
  private val repetition = P(numeric_expression)
  private val simple_expression: P[ast.Expression] = P((term ~ (add_like_op ~ term).rep))
  private val simple_factor: P[ast.Expression] = P(
    aggregate_initializer |
      entity_constructor |
      enumeration_reference |
      interval |
      query_expression |
      (unary_op.? ~ ("(" ~ expression ~ ")" | primary)).map({
        case (Some(op), expr) => ast.UnaryOperation(op, expr)
        case (_, expr)        => expr
      }))
  private val term: P[ast.Expression] = P(factor ~ (multiplication_like_op ~ factor).rep)
  private val unary_op: P[ast.UnaryOp] = P(
    "+".to(ast.PositiveOp) | "-".to(ast.NegativeOp) | NOT.to(ast.NotOp))
}