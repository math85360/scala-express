package com.iz2use.express.generator

import com.iz2use.express.p11.ast
import com.iz2use.express.p11.parser
import ScalaDefinition._
import utest._

object GenerateIfcRoot extends TestSuite {
  implicit def context = TransformerContext("com.iz2use.express.generated.schema")
  val tests = TestSuite {
    import ScalaDefinition.universe._

    /*import scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox
    val toolbox = currentMirror.mkToolBox()*/
    'IfcRoot{
      val entity =
        ast.EntityDeclaration("IfcRoot", Some(ast.AbstractSupertypeDeclaration(Some(ast.SupertypeOneOf(Seq(ast.UserDefinedEntity("")))))), Nil, Seq(
          ast.ExplicitAttribute(ast.SimpleAttributeName("GlobalId"), false, ast.UserDefinedEntityOrType("IfcGloballyUniqueId")),
          ast.ExplicitAttribute(ast.SimpleAttributeName("OwnerHistory"), true, ast.UserDefinedEntityOrType("IfcOwnerHistory")),
          ast.ExplicitAttribute(ast.SimpleAttributeName("Name"), true, ast.UserDefinedEntityOrType("IfcLabel")),
          ast.ExplicitAttribute(ast.SimpleAttributeName("Description"), true, ast.UserDefinedEntityOrType("IfcText"))), Nil, Nil, Nil, None)
      val result = Transformer(entity).map(showCode(_))
      val expected = (ScalaDefinition.defaultImports ++ Seq(
        q"""trait IfcRootAbstract {
  def globalId: IfcGloballyUniqueId
  def ownerHistory: Option[IfcOwnerHistory]
  def name: Option[IfcLabel]
  def description: Option[IfcText]
}""",
        q"""object IfcRoot"""))
        .map(showCode(_))
      //assert(result == expected)
    }

    'IfcRecurrencePattern{
      val entity =
        ast.EntityDeclaration("IfcRecurrencePattern", None, Nil,
          Seq(
            ast.ExplicitAttribute(ast.SimpleAttributeName("RecurrenceType"), false, ast.UserDefinedEntityOrType("IfcRecurrenceTypeEnum")),
            ast.ExplicitAttribute(ast.SimpleAttributeName("DayComponent"), true, ast.SetType(Some(ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Unknown)), ast.UserDefinedEntityOrType("IfcDayInMonthNumber"))),
            ast.ExplicitAttribute(ast.SimpleAttributeName("WeekdayComponent"), true, ast.SetType(Some(ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Unknown)), ast.UserDefinedEntityOrType("IfcDayInWeekNumber"))),
            ast.ExplicitAttribute(ast.SimpleAttributeName("MonthComponent"), true, ast.SetType(Some(ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Unknown)), ast.UserDefinedEntityOrType("IfcMonthInYearNumber"))),
            ast.ExplicitAttribute(ast.SimpleAttributeName("Position"), true, ast.UserDefinedEntityOrType("IfcInteger")),
            ast.ExplicitAttribute(ast.SimpleAttributeName("Interval"), true, ast.UserDefinedEntityOrType("IfcInteger")),
            ast.ExplicitAttribute(ast.SimpleAttributeName("Occurrences"), true, ast.UserDefinedEntityOrType("IfcInteger")),
            ast.ExplicitAttribute(ast.SimpleAttributeName("TimePeriods"), true, ast.ListType(Some(ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Unknown)), false, ast.UserDefinedEntityOrType("IfcTimePeriod")))),
          Nil, Nil, Nil, None)

      val result = Transformer(entity).map(showCode(_))
      val expected = (ScalaDefinition.defaultImports ++ Seq(
        q"""final case class IfcRecurrencePattern(
  val recurrenceType: IfcRecurrenceTypeEnum,
  val dayComponent: Option[Set[IfcDayInMonthNumber] Refined NonEmpty] = None,
  val weekdayComponent: Option[Set[IfcDayInWeekNumber] Refined NonEmpty] = None,
  val monthComponent: Option[Set[IfcMonthInYearNumber] Refined NonEmpty] = None,
  val position: Option[IfcInteger] = None,
  val interval: Option[IfcInteger] = None,
  val occurrences: Option[IfcInteger] = None,
  val timePeriods: Option[List[IfcTimePeriod] Refined NonEmpty] = None
)""",
        q"""object IfcRecurrencePattern"""))
        .map(showCode(_))
      //assert(result == expected)
    }
  }
}