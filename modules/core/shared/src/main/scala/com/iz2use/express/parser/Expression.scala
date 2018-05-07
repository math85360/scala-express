package com.iz2use.express.parser

import com.iz2use.express.ast
import scala.language.postfixOps
import fastparse.all._
import com.iz2use.express.ast.BuiltInFunction.Acos

trait Expression extends Literal {
  import BasicAlphabetDefinition._

  private[parser] val actual_parameter_list: P[Seq[ast.Expression]] = P(("(" ~/ parameter.nonEmptyList.? ~ ")"))
  private val add_like_op: P[ast.AddLikeOp] = P(
    "+".to(ast.AdditionOp) | "-".to(ast.SubtractionOp) |
      OR.to(ast.OrOp) | XOR.to(ast.XorOp))
  private val aggregate_initializer: P[ast.AggregateInitializer] = P(("[" ~ element.nonEmptyList ~ "]")
    .map(ast.AggregateInitializer.apply))
  private val aggregate_source = P(simple_expression)
  private[parser] val attribute_qualifier: P[ast.AttributeQualifier] = P(("." ~ attribute_ref.map(_.name))
    .map(ast.AttributeQualifier))
  private[parser] val bound_1 = P(numeric_expression)
  private[parser] val bound_2 = P(numeric_expression)
  private[parser] val bound_spec = P((Symbol("[") ~|?~/ bound_1 ~|?~ Symbol(":") ~|?~/ bound_2 ~|?~ Symbol("]", true))
    .map(ast.Bounds.tupled))
  private val built_in_constant = P(((
    CONST_E.to(ast.BuiltInConstant.E) |
    PI.to(ast.BuiltInConstant.PI) |
    SELF.to(ast.BuiltInConstant.Self) |
    "?".to(ast.BuiltInConstant.Unknown)) ~/))

  private val built_in_function: P[ast.BuiltInFunction] = P(((
    ABS.to(ast.BuiltInFunction.Abs) |
    ACOS.to(ast.BuiltInFunction.Acos) |
    ASIN.to(ast.BuiltInFunction.Asin) |
    ATAN.to(ast.BuiltInFunction.Atan) |
    BLENGTH.to(ast.BuiltInFunction.BLength) |
    COS.to(ast.BuiltInFunction.Cos) |
    EXISTS.to(ast.BuiltInFunction.Exists) |
    EXP.to(ast.BuiltInFunction.Exp) |
    FORMAT.to(ast.BuiltInFunction.Format) |
    HIBOUND.to(ast.BuiltInFunction.HiBound) |
    HIINDEX.to(ast.BuiltInFunction.HiIndex) |
    LENGTH.to(ast.BuiltInFunction.Length) |
    LOBOUND.to(ast.BuiltInFunction.LoBound) |
    LOINDEX.to(ast.BuiltInFunction.LoIndex) |
    LOG10.to(ast.BuiltInFunction.Log10) |
    LOG2.to(ast.BuiltInFunction.Log2) |
    LOG.to(ast.BuiltInFunction.Log) |
    NVL.to(ast.BuiltInFunction.Nvl) |
    ODD.to(ast.BuiltInFunction.Odd) |
    ROLESOF.to(ast.BuiltInFunction.RolesOf) |
    SIN.to(ast.BuiltInFunction.Sin) |
    SIZEOF.to(ast.BuiltInFunction.SizeOf) |
    SQRT.to(ast.BuiltInFunction.Sqrt) |
    TAN.to(ast.BuiltInFunction.Tan) |
    TYPEOF.to(ast.BuiltInFunction.TypeOf) |
    USEDIN.to(ast.BuiltInFunction.UsedIn) |
    VALUE_UNIQUE.to(ast.BuiltInFunction.ValueUnique) |
    VALUE_IN.to(ast.BuiltInFunction.ValueIn) |
    VALUE.to(ast.BuiltInFunction.Value)) ~/))
  private val constant_factor: P[ast.ConstantFactor] = P(built_in_constant | constant_ref.map(_.name).map(ast.UserDefinedConstant))
  private[parser] val domain_rule: P[ast.DomainRule] = P(((rule_label_id.map(_.name) ~ ":" ~/).? ~/ expression)
    .map(ast.DomainRule.tupled))
  private val element: P[ast.Expression] = P((expression ~ (":" ~ repetition).?)
    .map {
      case (expr, Some(count)) => ast.Repetition(expr, count)
      case (expr, None)        => expr
    })
  private val entity_constructor: P[ast.FunctionCallOrEntityConstructor] = P((entity_ref.map(_.name).map(ast.UserDefinedFunctionOrEntityConstructor) ~ "(" ~ expression.nonEmptyList.? ~ ")")
    .map(ast.FunctionCallOrEntityConstructor.tupled))
  private val enumeration_reference: P[ast.EnumerationReference] = P(((type_ref.map(_.name) ~ ".").? ~ enumeration_ref.map(_.name))
    .map(ast.EnumerationReference.tupled))
  private[parser] val expression: P[ast.Expression] = P((simple_expression ~ (rel_op_extended ~/ simple_expression).?))
  private val factor: P[ast.Expression] = P((simple_factor ~ ("**".to(ast.PowerOp) ~/ simple_factor).?))
  private val function_call: P[ast.FunctionCallOrEntityConstructor] = P(((built_in_function | function_ref.map(_.name).map(ast.UserDefinedFunctionOrEntityConstructor)) ~ actual_parameter_list.?)
    .map(ast.FunctionCallOrEntityConstructor.tupled))
  private[parser] val group_qualifier: P[ast.GroupQualifier] = P(("\\" ~ entity_ref.map(_.name))
    .map(ast.GroupQualifier))
  private val index_qualifier: P[ast.IndexQualifier] = P(("[" ~ index_1 ~ (":" ~ index_2).? ~ "]")
    .map(ast.IndexQualifier.tupled))
  private val index = P(numeric_expression)
  private val index_1 = P(index)
  private val index_2 = P(index)
  private val interval: P[ast.Interval] = P(("{" ~ interval_low ~ interval_op ~ interval_item ~ interval_op ~ interval_high ~ "}")
    .map(ast.Interval.tupled))
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
  private val qualifiable_factor: P[ast.QualifiableFactor] = P((
    function_call |
    constant_factor.map({
      case c: ast.BuiltInConstant        => c
      case ast.UserDefinedConstant(name) => ast.UserDefinedFactor(name)
    }) |
    attribute_ref.map(r => ast.UserDefinedFactor(r.name)) |
    general_ref.map(r => ast.UserDefinedFactor(r.name)) |
    population.map(r => ast.UserDefinedFactor(r.name))))
  private[parser] val qualifier: P[ast.Qualifier] = P(attribute_qualifier | group_qualifier | index_qualifier)
  private val query_expression: P[ast.QueryExpression] = P((QUERY ~/ "(" ~/ variable_id.map(_.name) ~ "<*" ~/ aggregate_source ~ "|" ~/ logical_expression ~ ")")
    .map(ast.QueryExpression.tupled))
  private val rel_op = P(
    "<=".to(ast.LE) | ">=".to(ast.GE) |
      "<>".to(ast.NE) | "=".to(ast.EQ) |
      ("<" ~ !"*").to(ast.LT) | ">".to(ast.GT) |
      ":<>:".to(ast.TN) | ":=:".to(ast.TE))
  private val rel_op_extended: P[ast.RelOpExtended] = P(rel_op | IN.to(ast.IN) | LIKE.to(ast.LIKE))
  private val repetition = P(numeric_expression)
  private val simple_expression: P[ast.Expression] = P((term ~ (add_like_op ~/ term).rep))
  private val simple_factor: P[ast.Expression] = P((
    query_expression |
    (unary_op.? ~ ("(" ~ expression ~ ")" | primary)).map({
      case (Some(op), expr) => ast.UnaryOperation(op, expr)
      case (_, expr)        => expr
    }) |
    aggregate_initializer |
    entity_constructor |
    enumeration_reference |
    interval))
  private val term: P[ast.Expression] = P((factor ~ (multiplication_like_op ~/ factor).rep))
  private val unary_op: P[ast.UnaryOp] = P(
    "+".to(ast.PositiveOp) | "-".to(ast.NegativeOp) | NOT.to(ast.NotOp))
}