package com.iz2use.express.parser

import com.iz2use.express.ast
import scala.language.postfixOps
import IgnoringParts._
import fastparse.noApi._

trait Types extends Expression {

  private val aggregate_type: P[ast.AggregateType] = P((AGGREGATE ~ (":" ~ type_label).? ~ OF ~ parameter_type)
    .map(ast.AggregateType.tupled))
  private val aggregation_types: P[ast.AggregationType[ast.InstantiableType]] = P(array_type | bag_type | list_type | set_type)

  private val array_type = P(ARRAY ~ bound_spec.map(Some(_)) ~ OF ~ OPTIONAL.? ~ UNIQUE.? ~ instantiable_type)
    .map((ast.ArrayType[ast.InstantiableType] _).tupled)

  private val bag_type = P(BAG ~ bound_spec.? ~ OF ~ instantiable_type)
    .map((ast.BagType[ast.InstantiableType] _).tupled)
  private val binary_type = P(BINARY ~ width_spec.?)
    .map(ast.BinaryType.apply)
  private val boolean_type = P(BOOLEAN)
    .to(ast.BooleanType)

  private[parser] val concrete_types: P[ast.ConcreteType] = P(aggregation_types | simple_types | type_ref)

  private val generalized_types: P[ast.GeneralizedType] = P(aggregate_type | general_aggregation_types | generic_entity_type | generic_type)
  private val general_aggregation_types: P[ast.AggregationType[ast.ParameterType]] = P(general_array_type | general_bag_type | general_list_type | general_set_type)
  private val general_array_type = P((ARRAY ~ bound_spec.? ~ OF ~ OPTIONAL.? ~ UNIQUE.? ~ parameter_type)
    .map((ast.ArrayType[ast.ParameterType] _).tupled))
  private val general_bag_type = P((BAG ~ bound_spec.? ~ OF ~ parameter_type)
    .map((ast.BagType[ast.ParameterType] _).tupled))
  private val general_list_type = P((LIST ~ bound_spec.? ~ OF ~ UNIQUE.? ~ parameter_type)
    .map((ast.ListType[ast.ParameterType] _).tupled))

  private val general_set_type = P((SET ~ bound_spec.? ~ OF ~ parameter_type)
    .map((ast.SetType[ast.ParameterType] _).tupled))

  private val generic_entity_type: P[ast.GenericEntityType] = P((GENERIC_ENTITY ~ (":" ~ type_label).?)
    .map(ast.GenericEntityType))
  private val generic_type: P[ast.GenericType] = P((GENERIC ~ (":" ~ type_label).?)
    .map(ast.GenericType))

  private[parser] val instantiable_type: P[ast.InstantiableType] = P(concrete_types | entity_ref)
  private val integer_type = P(INTEGER)
    .to(ast.IntegerType)

  private val list_type = P(LIST ~ bound_spec.? ~ OF ~ UNIQUE.? ~ instantiable_type)
    .map((ast.ListType[ast.InstantiableType] _).tupled)

  private val logical_type = P(LOGICAL)
    .to(ast.LogicalType)

  private[parser] val named_types: P[ast.NamedType] = P(entity_ref | type_ref)
  private[parser] val named_type_or_rename: P[ast.NamedTypeOrRename] = P((named_types ~ (AS ~ (entity_id | type_id)).?)
    .map({
      case (namedType, None)     => namedType
      case (namedType, Some(as)) => ast.RenamedType(namedType, as)
    }))

  private val number_type = P(NUMBER)
    .to(ast.NumberType)

  private[parser] val parameter_type: P[ast.ParameterType] = P(generalized_types | named_types | simple_types)
  private val precision_spec = P(numeric_expression)

  private val real_type = P(REAL ~ ("(" ~ precision_spec ~ ")").?)
    .map(ast.RealType.apply)

  private val set_type = P(SET ~ bound_spec.? ~ OF ~ instantiable_type)
    .map((ast.SetType[ast.InstantiableType] _).tupled)

  private val simple_types: P[ast.SimpleType] = P(binary_type | boolean_type | integer_type | logical_type | number_type | real_type | string_type)

  private val string_type = P(STRING ~ width_spec.?)
    .map(ast.StringType.apply)

  private val type_label: P[String] = P(type_label_id | type_label_ref)

  private val width = P(numeric_expression)
  private val width_spec = P("(" ~ width ~ ")" ~ FIXED.?)
    .map(ast.Width.tupled.apply)
}