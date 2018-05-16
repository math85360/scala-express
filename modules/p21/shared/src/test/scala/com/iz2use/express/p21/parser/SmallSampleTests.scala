package com.iz2use.express.p21.parser

import com.iz2use.express.syntax._
import com.iz2use.express.p21._
import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.boolean._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import shapeless.{ HList, ::, HNil }
import shapeless.{ Coproduct, :+:, CNil }
import utest._
import scala.util.Right

object SmallSampleTests extends TestSuite {
  val tests = TestSuite {
    def t[A](todo: A, expected: String)(implicit enc: Encoder[A], dec: Decoder[A]) = {
      val result = Encoder[A].apply(todo)
      assert(result.toString() == expected)
    }
    'string{
      t("", "''")
      t("test", "'test'")
      t("?", "'?'")
    }
    'OptionString{
      t(None, "$")
      t(Some("a"), "'a'")
      t(Some("?"), "'?'")
    }
    'RefTo{
      t(RefTo(50), "#50")
    }
    'IfcTime{
      type IfcTime = String
      abstract class IfcTimePeriod {
        def startTime: IfcTime
        def endTime: IfcTime
      }
      object IfcTimePeriod {
        implicit val encoder: Encoder[IfcTimePeriod] = ObjectEncoder.toHList("IfcTimePeriod")((c: IfcTimePeriod) =>
          c.startTime :: c.endTime :: HNil)
        implicit val decoder: Decoder[IfcTimePeriod] = Decoder[Repr].map(r => IfcTimePeriod(r.at(0), r.at(1)))
        def apply(startTime: IfcTime, endTime: IfcTime): IfcTimePeriod = {
          val _1 = startTime
          val _2 = endTime
          new IfcTimePeriod {
            val startTime: IfcTime = _1
            val endTime: IfcTime = _2
          }
        }
        type Repr = IfcTime :: IfcTime :: HNil
      }
      val v1 = IfcTimePeriod("start", "end")
      val encoded = Encoder[IfcTimePeriod].apply(v1)
      assert(encoded.toString() == "('start','end')")

      val decoded = Decoder[IfcTimePeriod].apply(new cursor.TopCursor(encoded)(null, null))
      val Right(v2) = decoded
      assert(v2.startTime == v1.startTime && v2.endTime == v1.endTime)
    }
  }
}