package com.iz2use.express.generator

import com.iz2use.express.ast
import com.iz2use.express.parser
import ScalaDefinition._
import utest._

object GenerateIfcRoot extends TestSuite {
  val tests = TestSuite {
    import ScalaDefinition.universe._

    /*import scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox
    val toolbox = currentMirror.mkToolBox()*/
    'IfcRoot{
      val entity =
        ast.EntityDeclaration("IfcRoot", Some(ast.AbstractSupertypeDeclaration(Some(ast.SupertypeOneOf(Seq(ast.UserDefinedEntity("")))))), None, Seq(
          ast.ExplicitAttribute(ast.SimpleAttributeName("GlobalId"), false, ast.UserDefinedEntityOrType("IfcGloballyUniqueId")),
          ast.ExplicitAttribute(ast.SimpleAttributeName("OwnerHistory"), true, ast.UserDefinedEntityOrType("IfcOwnerHistory")),
          ast.ExplicitAttribute(ast.SimpleAttributeName("Name"), true, ast.UserDefinedEntityOrType("IfcLabel")),
          ast.ExplicitAttribute(ast.SimpleAttributeName("Description"), true, ast.UserDefinedEntityOrType("IfcText"))), Nil, Nil, Nil, None)
      val result = showCode(Transformer(entity))
      val expected = showCode(q"""package `com.iz2use.express.generated.schema` {
trait IfcRoot {
  def globalId: IfcGloballyUniqueId
  def ownerHistory: Option[IfcOwnerHistory]
  def name: Option[IfcLabel]
  def description: Option[IfcText]
}
object IfcRoot {
}
}""")
      assert(result == expected)
    }

    'IfcRecurrencePattern{
      val entity =
        ast.EntityDeclaration("IfcRecurrencePattern", None, None,
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

val result = showCode(Transformer(entity))
val expected = showCode(q"""package `com.iz2use.express.generated.schema` {
class IfcRecurrencePattern {
  def recurrenceType: IfcRecurrenceTypeEnum
  def dayComponent: Option[Set[IfcDayInMonthNumber] Refined NonEmpty]
  def weekdayComponent: Option[Set[IfcDayInWeekNumber] Refined NonEmpty]
  def monthComponent: Option[Set[IfcMonthInYearNumber] Refined NonEmpty]
  def position: Option[IfcInteger]
  def interval: Option[IfcInteger]
  def occurrences: Option[IfcInteger]
  def timePeriods: Option[List[IfcTimePeriod] Refined NonEmpty]
}
object IfcRecurrencePattern {
}
}""")
assert(result == expected)
    }
  }
}