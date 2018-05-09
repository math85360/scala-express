package com.iz2use.express.generator

import com.iz2use.express.ast
import com.iz2use.express.parser.Parser
import fastparse.all._
import ScalaDefinition._
import utest._
import scala.io.Source
import scala.collection.Seq
import java.io.File
import java.io.PrintWriter

object ParseAndGenerate extends TestSuite {
  val tests = TestSuite {
    import ScalaDefinition.universe.showCode
    val source = Source.fromInputStream(getClass.getResourceAsStream("/IFC4.exp")).mkString

    val Parsed.Success(Seq(ast.Schema(_, _, items)), _) = Parser.root.parse(source)

    items foreach { e =>
      val transformed = showCode(Transformer(e))
      val name = e match {
        case ast.EntityDeclaration(name, _, _, _, _, _, _, _) => name
        case ast.TypeDeclaration(name, _, _)                  => name
        case ast.FunctionDeclaration(name, _, _, _, _)        => name
        case ast.RuleDeclaration(name, _, _, _, _)            => name
      }
      val file = new File(s"target/$name.scala")
      val pw = new PrintWriter(file)
      pw.write(transformed)
      pw.close
      assert(transformed != "")
    }

    /*'Types{
      items collect {
        case e: ast.TypeDeclaration =>
          val transformed = showCode(Transformer(e))
          assert(transformed != "")
      }
    }
    'Entites{
      items collect {
        case e: ast.EntityDeclaration =>
          //e.name - {
          val transformed = showCode(Transformer(e))
          assert(transformed != "")
        //}
      }
    }
    'Functions{
      items collect {
        case e: ast.FunctionDeclaration =>
          //e.name - {
          val transformed = showCode(Transformer(e))
          assert(transformed != "")
        //}
      }
    }
    'Rules{
      items collect {
        case e: ast.RuleDeclaration =>
          //e.ruleId - {
            val transformed = showCode(Transformer(e))
            assert(transformed != "")
          //}
      }
    }*/
  }
}