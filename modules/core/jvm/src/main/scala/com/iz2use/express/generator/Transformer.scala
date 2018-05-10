package com.iz2use.express.generator

import com.iz2use.express.ast
import com.iz2use.express.ast.SelectType
import scala.annotation.tailrec

trait Dictionary {
  def add[A](value: A): Unit
}
case class TransformerContext(target: String = ("com.iz2use.express.generated.schema")) {
  //private var names: Map[String, ast.SchemaBody] = Map.empty
  private var stack: List[Map[String, Any]] = List(Map.empty)
  def register[A](key: String, value: A): Unit = {
    if (key.nonEmpty) {
      val newMap = stack.head + (key -> value)
      stack = newMap :: stack.tail
    }
  }
  def pushContext: Unit = stack = Map.empty[String, Any] :: stack
  def popContext: Unit = stack = stack.tail
}
object TransformerContext {
  implicit def ctx = TransformerContext()
}
abstract class Transformer[-A, B] {
  def addToDictionary(a: A)(implicit context: TransformerContext): Unit
  def transform(a: A)(implicit context: TransformerContext): B
}
object Transformer {
  def addToDictionary[A, B](a: A)(implicit trsf: Transformer[A, B], context: TransformerContext): Unit = trsf.addToDictionary(a)
  def apply[A, B](a: A)(implicit trsf: Transformer[A, B], context: TransformerContext): B = trsf.transform(a)
  def instance[A, B](f: TransformerContext => A => B) = new Transformer[A, B] {
    def addToDictionary(a: A)(implicit context: TransformerContext): Unit = {}
    def transform(a: A)(implicit context: TransformerContext): B = {
      context.pushContext
      val r = f(context)(a)
      context.popContext
      r
    }
  }
  def typeInstance[A, B](returnName: A => String)(f: TransformerContext => A => B) = new Transformer[A, B] {
    def addToDictionary(a: A)(implicit context: TransformerContext): Unit = context.register(returnName(a), a)
    def transform(a: A)(implicit context: TransformerContext): B = {
      context.pushContext
      val r = f(context)(a)
      context.popContext
      r
    }
  }
}

trait ScalaDefinition
case class ScalaTrait(name: String, body: Seq[BodyDef]) extends ScalaDefinition
trait BodyDef
case class ScalaDef(name: String, args: Seq[Nothing], tpe: String) extends BodyDef
case class ScalaClass() extends ScalaDefinition
case class ScalaType() extends ScalaDefinition

object ScalaDefinition {
  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  import universe.{ Transformer => _, _ }

  val valNameTransformer: String => universe.TermName = { name =>
    TermName(name.head.toLower +: name.tail)
  }

  implicit val attributeNameTransformer: Transformer[ast.AttributeName, universe.TermName] = Transformer.instance { implicit ctx =>
    {
      case ast.SimpleAttributeName(name) => valNameTransformer(name)
    }
  }

  implicit val boundsTransformer: Transformer[ast.Bounds, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Unknown) =>
        tq"""NonEmpty"""
      case ast.Bounds(ast.IntegerLiteral(min), ast.IntegerLiteral(max)) =>
        tq"""And[MinSize, MaxSize]"""
      case ast.Bounds(ast.IntegerLiteral(min), ast.BuiltInConstant.Unknown) =>
        tq"""MinSize"""
      case _ =>
        tq"""NotFound"""
      /*case ast.Bounds(ast.IntegerLiteral(min), ast.BuiltInConstant.Unknown) =>
q"""MinSize[W.`$min`.T]"""*/
      //    case ast.Bounds(ast.IntegerLiteral(min), ast.IntegerLiteral(max))     =>
    }
  }
  def composeWithBounds(tpe: universe.Tree, boundsOpt: Option[ast.Bounds])(implicit ctx: TransformerContext) = boundsOpt match {
    case None => tpe
    case Some(bounds) =>
      val tpt = Transformer(bounds)
      tq"""$tpe Refined $tpt"""
  }
  implicit val rootTypeTransformer: Transformer[ast.RootType, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      /* case t:ast.InstantiableType =>
    case t:ast.ParameterType =>
  }
  implicit val parameterTypeTransformer = Transformer.instance[ast.ParameterType, universe.Tree]
  {*/
      case ast.ArrayType(boundsOpt, optionalAllowed, uniqueOnly, of) =>
        val tpt = Transformer(of)
        composeWithBounds(tq"""Array[$tpt]""", boundsOpt)
      case ast.BagType(boundsOpt, of) =>
        val tpt = Transformer(of)
        composeWithBounds(tq"""Seq[$tpt]""", boundsOpt)
      case ast.BinaryType(width) =>
        tq"""Binary"""
      case ast.BooleanType =>
        tq"""Boolean"""
      case ast.GenericType(Some(label)) =>
        Ident(TermName(label))
      case ast.IntegerType =>
        tq"""Int"""
      case ast.ListType(boundsOpt, unique, of) =>
        val tpt = Transformer(of)
        composeWithBounds(tq"""List[$tpt]""", boundsOpt)
      case ast.LogicalType =>
        tq"""Logical"""
      case ast.NumberType =>
        tq"""Double"""
      case ast.RealType(width) =>
        tq"""Double"""
      case ast.SetType(boundsOpt, of) =>
        val tpt = Transformer(of)
        composeWithBounds(tq"""Set[$tpt]""", boundsOpt)
      case ast.StringType(maxLength) =>
        /*maxLength match {
  case None => tq"""String"""
  case Some(length) => tq"""String Refined MaxSize"""
}*/
        tq"""String"""
      case ast.UserDefinedEntity(tpe)       => Ident(TermName(tpe))
      case ast.UserDefinedEntityOrType(tpe) => Ident(TermName(tpe))
      case ast.UserDefinedType(tpe)         => Ident(TermName(tpe))
    }
  }
  //implicit val parameterType: Transformer[ast.ParameterType,universe.Tree] = aggregationTypeLevel
  implicit val attributeTransformer: Transformer[ast.ExplicitAttribute, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case ast.ExplicitAttribute(attributeName, optional, tpe) =>
        val defName = Transformer(attributeName)
        val tpt = Transformer(tpe)
        val optionalType = optional match {
          case false => tpt
          case true  => q"Option[$tpt]"
        }
        q"""def $defName : $optionalType"""
    }
  }
  implicit val entityTransformer = Transformer.typeInstance[ast.EntityDeclaration, universe.Tree](_.name) { implicit ctx =>
    { entity =>
      val tname = TypeName(entity.name)
      val stats = entity.attributes.map(Transformer(_))
      val whereRules = entity.whereClause match {
        case Some(ast.WhereClause(whereClauses)) =>
          var defaultNumber: Int = 0
          whereClauses flatMap {
            case ast.DomainRule(nameOpt, expr) =>
              val whereRule = nameOpt.getOrElse({
                defaultNumber += 1
                s"WhereRule$defaultNumber"
              })
              val whereRuleType = TypeName(whereRule)
              val whereRuleName = valNameTransformer(whereRule)
              Seq(
                q"""final case class $whereRuleType()""",
                q"""val $whereRuleName : Validate[$tname, $whereRuleType] = Validate.fromPredicate(e => ???, e => ???, ${TermName(whereRule)}())""")
          }
        case None => Nil
      }
      val cname = TermName(entity.name)
      if (entity.supertype.isDefined)
        q"""package ${TermName(ctx.target)} {

  trait $tname {
  ..$stats
  }
  object $cname {
  ..$whereRules
  }
}"""
      else
        q"""package ${TermName(ctx.target)} {
      
  class $tname {
  ..$stats
  }
  object $cname {
  ..$whereRules
  }
}"""
    }
  }

  object HasGenericType {
    def unapply(tpe: ast.AggregationTypeLevel): Option[String] = {
      @tailrec def loop(e: ast.AggregationTypeLevel): Option[String] = e match {
        case ast.GenericType(Some(name)) => Some(name)
        case t: ast.AggregationType[_]   => loop(t.of)
        case _                           => None
      }
      loop(tpe)
    }
  }

  implicit val expressionTransformer: Transformer[ast.Expression, universe.Tree] = Transformer.instance { implicit ctx => _ =>
    q""" null  """
  }
  implicit val logicalExpressionTransformer: Transformer[ast.LogicalExpression, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case e: ast.Expression => expressionTransformer.transform(e)
    }
  }

  implicit val statementTransformer: Transformer[ast.Statement, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case ast.IfStatement(condition, ifPass, ifFailed) =>
        q"""if(${Transformer(condition)}) {
          ..${ifPass.map(Transformer(_))}
        }else{
          ..${ifFailed.map(Transformer(_))}
        }"""
      case ast.ReturnStatement(expr) =>
        expr match {
          case None       => q"""return """
          case Some(expr) => q"""return ${Transformer(expr)}"""
        }
      case _ =>
        q""" null  """
    }
  }

  val algorithmHeadTransformer: Transformer[ast.AlgorithmHeadPart, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case ast.ConstantDeclaration(name, tpe, expr) =>
        ctx.register(name, tpe)
        q"""val ${TermName(name)} : ${Transformer(tpe)} = ${Transformer(expr)} """
      case ast.LocalDeclaration(name, tpe, expr) =>
        ctx.register(name, tpe)

        expr match {
          case None       => q"""var ${TermName(name)} : ${Transformer(tpe)} =_"""
          case Some(expr) => q"""var ${TermName(name)} : ${Transformer(tpe)} = ${Transformer(expr)}"""
        }
    }
  }

  implicit val functionTransformer: Transformer[ast.FunctionDeclaration, universe.Tree] = Transformer.typeInstance[ast.FunctionDeclaration, universe.Tree](_.name) { implicit ctx =>
    { function =>
      val name = TermName(function.name)
      val tpt = Transformer(function.tpe)
      val genericTpts = function.parameters.collect({
        case ast.Parameter(_, HasGenericType(name)) =>
          name
      }).distinct.map(name => TypeDef(Modifiers(), TypeName(name), Nil, q""))
      val paramss = function.parameters.map({
        case ast.Parameter(name, argTpe) =>
          val argName = valNameTransformer(name)
          q"""val $argName : ${Transformer(argTpe)}"""
      })
      val head = function.head.map(Transformer(_)(algorithmHeadTransformer, ctx))
      val body = function.body.map(Transformer(_))
      q"""package ${TermName(ctx.target)} {
  object $name {
    def apply[..$genericTpts](..$paramss): $tpt = {
      ..$head
      ..$body
    }
  }
}"""
    }
  }

  implicit val schemaTransformer: Transformer[ast.Schema, Seq[(String, universe.Tree)]] = Transformer.instance { implicit ctx =>
    { schema =>
      schema.body.foreach(Transformer.addToDictionary(_))

      val name = TermName(schema.id)

      val types = schema.body collect {
        case e: ast.TypeDeclaration => Transformer(e)
      }

      val rules = Seq[universe.Tree]()

      val mainDeclaration = "package" -> q"""package ${TermName(ctx.target)} {

  package object $name {
    ..$types
    ..$rules
  }
}"""

      val independentDeclarations = schema.body.collect {
        case e @ ast.EntityDeclaration(name, _, _, _, _, _, _, _) => name -> Transformer(e)
        case e @ ast.FunctionDeclaration(name, _, _, __, _)       => name -> Transformer(e)
      }

      mainDeclaration +: independentDeclarations
    }
  }

  implicit val typeTransformer = Transformer.typeInstance[ast.TypeDeclaration, universe.Tree](_.name) { implicit ctx =>
    { tpe =>
      val name = TypeName(tpe.name)
      tpe.underlyingType match {
        case ast.SelectType(extensible, from) =>
          q"""type $name"""
        case ast.EnumerationType(extensible, items) =>
          items match {
            case None =>
            case Some(Left(items)) =>
            case Some(Right(ast.BasedOnEnumeration(name, additionalItems))) =>
          }
          val tpt = tq"""Enumeration"""
          q"""type $name = $tpt"""
        case e: ast.ConcreteType =>
          val tpt = Transformer(e)
          q"""type $name = $tpt"""
      }
    }
  }

  implicit val schemaBodyTransformer = Transformer.instance[ast.SchemaBody, universe.Tree] { implicit ctx =>
    {
      case e: ast.EntityDeclaration   => Transformer(e)
      case e: ast.FunctionDeclaration => Transformer(e)
      case e: ast.RuleDeclaration     => q""" def test() : Unit = {} """
      case e: ast.TypeDeclaration     => Transformer(e)
    }
  }
}