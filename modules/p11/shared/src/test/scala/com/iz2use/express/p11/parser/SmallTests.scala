package com.iz2use.express.p11.parser

import com.iz2use.express.p11.ast._
import BasicAlphabetDefinition._
import fastparse.all._
import utest._
import scala.collection.Seq

object SmallTests extends TestSuite {
  val tests = TestSuite {
    'Constant{

    }
    'EntityIfcActor{
      val parsed = P(Parser.entity_decl ~ End).parse(
        """ENTITY IfcActor
 SUPERTYPE OF (ONEOF
	(IfcOccupant))
 SUBTYPE OF (IfcObject);
	TheActor : IfcActorSelect;
 INVERSE
	IsActingUpon : SET [0:?] OF IfcRelAssignsToActor FOR RelatingActor;
END_ENTITY;""")
      parsed match {
        case f @ Parsed.Failure(a, b, c) =>
          println(c.traced.fullStack.mkString("\n"))
        case _ =>
      }
      assertMatch(parsed) {
        case Parsed.Success(EntityDeclaration("IfcActor", Some(_), Seq("IfcObject"), _, _, _, _, _), _) =>
      }
    }
    'EntityIfcRecurrencePattern{
      val parsed = P(Parser.entity_decl ~ End).parse(
        """ENTITY IfcRecurrencePattern;
	RecurrenceType : IfcRecurrenceTypeEnum;
	DayComponent : OPTIONAL SET [1:?] OF IfcDayInMonthNumber;
	WeekdayComponent : OPTIONAL SET [1:?] OF IfcDayInWeekNumber;
	MonthComponent : OPTIONAL SET [1:?] OF IfcMonthInYearNumber;
	Position : OPTIONAL IfcInteger;
	Interval : OPTIONAL IfcInteger;
	Occurrences : OPTIONAL IfcInteger;
	TimePeriods : OPTIONAL LIST [1:?] OF IfcTimePeriod;
END_ENTITY;""")
      assertMatch(parsed) {
        case Parsed.Success(EntityDeclaration("IfcRecurrencePattern", None, Nil, Seq(
          _,
          ExplicitAttribute(
            SimpleAttributeName(
              "DayComponent"),
            true,
            SetType(Some(Bounds(IntegerLiteral("1"), BuiltInConstant.Unknown)), UserDefinedEntityOrType("IfcDayInMonthNumber"))),
          _*), _, _, _, _), _) =>
      }
    }
    'Function{
    }
    'Procedure{
    }
    'Reference{
    }
    'Rule{
    }
    'Type{
      val parsed = P(Parser.type_decl.rep(1, spaceOrComments.?) ~ End).parse(
        """TYPE IfcAngularVelocityMeasure = REAL;
END_TYPE;

TYPE IfcArcIndex = LIST [3:3] OF IfcPositiveInteger;
END_TYPE;

TYPE IfcBinary = BINARY;
END_TYPE;

TYPE IfcBoolean = BOOLEAN;
END_TYPE;

TYPE IfcBoxAlignment = IfcLabel; 
 WHERE
	WR1 : SELF IN ['top-left', 'top-middle', 'top-right', 'middle-left', 'center', 'middle-right', 'bottom-left', 'bottom-middle', 'bottom-right'];
END_TYPE;

TYPE IfcCardinalPointReference = INTEGER;
 WHERE
	GreaterThanZero : SELF > 0;
END_TYPE;

TYPE IfcComplexNumber = ARRAY [1:2] OF REAL;
END_TYPE;

TYPE IfcCompoundPlaneAngleMeasure = LIST [3:4] OF INTEGER;
 WHERE
	MinutesInRange : ABS(SELF[2]) < 60;
	SecondsInRange : ABS(SELF[3]) < 60;
	MicrosecondsInRange : (SIZEOF(SELF) = 3) OR (ABS(SELF[4]) < 1000000);
	ConsistentSign : ((SELF[1] >= 0) AND (SELF[2] >= 0) AND (SELF[3] >= 0) AND ((SIZEOF(SELF) = 3) OR (SELF[4] >= 0)))
OR
((SELF[1] <= 0) AND (SELF[2] <= 0) AND (SELF[3] <= 0) AND ((SIZEOF(SELF) = 3) OR (SELF[4] <= 0)));
END_TYPE;""")
      parsed match {
        case f @ Parsed.Failure(a, b, c) =>
          println(c.traced.fullStack.mkString("\n"))
        case _ =>
      }

      assertMatch(parsed) {
        case Parsed.Success(_, _) =>
      }
      val Parsed.Success(types, _) = parsed
      for (tpe <- types) assertMatch(tpe) {
        case TypeDeclaration("IfcAngularVelocityMeasure", RealType(None), None) =>
        case TypeDeclaration("IfcArcIndex", ListType(Some(_), false, UserDefinedType("IfcPositiveInteger")), _) =>
        case TypeDeclaration("IfcBinary", BinaryType(None), _) =>
        case TypeDeclaration("IfcBoolean", BooleanType, _) =>
        case TypeDeclaration("IfcBoxAlignment", UserDefinedType("IfcLabel"), _) =>
        case TypeDeclaration("IfcCardinalPointReference", IntegerType, Some(_)) =>
        case TypeDeclaration("IfcComplexNumber", ArrayType(Some(Bounds(_, _)), false, false, RealType(None)), None) =>
        case TypeDeclaration("IfcCompoundPlaneAngleMeasure", ListType(Some(Bounds(IntegerLiteral("3"), IntegerLiteral("4"))), false, IntegerType), _) =>
      }
    }
    'Use{
    }
  }
}