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
        q"""trait IfcRoot {
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
            ast.ExplicitAttribute(ast.SimpleAttributeName("DayComponent"), true, ast.SetType(Some(ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Indeterminate)), ast.UserDefinedEntityOrType("IfcDayInMonthNumber"))),
            ast.ExplicitAttribute(ast.SimpleAttributeName("WeekdayComponent"), true, ast.SetType(Some(ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Indeterminate)), ast.UserDefinedEntityOrType("IfcDayInWeekNumber"))),
            ast.ExplicitAttribute(ast.SimpleAttributeName("MonthComponent"), true, ast.SetType(Some(ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Indeterminate)), ast.UserDefinedEntityOrType("IfcMonthInYearNumber"))),
            ast.ExplicitAttribute(ast.SimpleAttributeName("Position"), true, ast.UserDefinedEntityOrType("IfcInteger")),
            ast.ExplicitAttribute(ast.SimpleAttributeName("Interval"), true, ast.UserDefinedEntityOrType("IfcInteger")),
            ast.ExplicitAttribute(ast.SimpleAttributeName("Occurrences"), true, ast.UserDefinedEntityOrType("IfcInteger")),
            ast.ExplicitAttribute(ast.SimpleAttributeName("TimePeriods"), true, ast.ListType(Some(ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Indeterminate)), false, ast.UserDefinedEntityOrType("IfcTimePeriod")))),
                                                      Nil, Nil, Nil, None)

      val result = Transformer(entity).map(showCode(_))
      val expected = (ScalaDefinition.defaultImports ++ Seq(
        q"""class IfcRecurrencePattern {
  def recurrenceType: IfcRecurrenceTypeEnum
  def dayComponent: Option[Set[IfcDayInMonthNumber] Refined NonEmpty]
  def weekdayComponent: Option[Set[IfcDayInWeekNumber] Refined NonEmpty]
  def monthComponent: Option[Set[IfcMonthInYearNumber] Refined NonEmpty]
  def position: Option[IfcInteger]
  def interval: Option[IfcInteger]
  def occurrences: Option[IfcInteger]
  def timePeriods: Option[List[IfcTimePeriod] Refined NonEmpty]
}""",
        q"""object IfcRecurrencePattern {
  def apply(
    recurrenceType: IfcRecurrenceTypeEnum,
    dayComponent: Option[Set[IfcDayInMonthNumber] Refined NonEmpty] = None,
    weekdayComponent: Option[Set[IfcDayInWeekNumber] Refined NonEmpty] = None,
    monthComponent: Option[Set[IfcMonthInYearNumber] Refined NonEmpty] = None,
    position: Option[IfcInteger] = None,
    interval: Option[IfcInteger] = None,
    occurrences: Option[IfcInteger] = None,
    timePeriods: Option[List[IfcTimePeriod] Refined NonEmpty] = None) : IfcRecurrencePattern = {
    val _1 = recurrenceType
    val _2 = dayComponent
    val _3 = weekdayComponent
    val _4 = monthComponent
    val _5 = position
    val _6 = interval
    val _7 = occurrences
    val _8 = timePeriods
    new IfcRecurrencePattern {
      val recurrenceType: IfcRecurrenceTypeEnum = _1
      val dayComponent: Option[Set[IfcDayInMonthNumber] Refined NonEmpty] = _2
      val weekdayComponent: Option[Set[IfcDayInWeekNumber] Refined NonEmpty] = _3
      val monthComponent: Option[Set[IfcMonthInYearNumber] Refined NonEmpty] = _4
      val position: Option[IfcInteger] = _5
      val interval: Option[IfcInteger] = _6
      val occurrences: Option[IfcInteger] = _7
      val timePeriods: Option[List[IfcTimePeriod] Refined NonEmpty] = _8
    }
  }
}
"""))
        .map(showCode(_))
      //assert(result == expected)
    }
  }
}
