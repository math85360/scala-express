package com.iz2use.express.p11.parser

import com.iz2use.express.p11.ast._
import fastparse.all._
import utest._
import scala.io.Source
import scala.collection.Seq

object Ifc4Test extends TestSuite {
  val tests = TestSuite {
    val source = Source.fromInputStream(getClass.getResourceAsStream("/IFC4.exp")).mkString
    val parsed = Parser.root.parse(source)
    parsed match {
      case f @ Parsed.Failure(a, b, c) =>
        println(c.traced.fullStack.mkString("\n"))
      case _ =>
    }
    assertMatch(parsed) {
      case Parsed.Success(Seq(Schema(_, _, _)), _) =>
    }
  }
}