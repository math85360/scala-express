package com.iz2use.express.generator

import com.iz2use.express.ast
import com.iz2use.express.ast.SelectType

trait Dictionary {
  def add[A](value: A): Unit
}

abstract class Transformer[-A, B] {
  def addToDictionary(a: A)(implicit dict: Dictionary): Unit
  def transform(a: A): B
}
object Transformer {
  def apply[A, B](a: A)(implicit trsf: Transformer[A, B]): B = trsf.transform(a)
  def instance[A, B](f: A => B) = new Transformer[A, B] {
    def addToDictionary(a: A)(implicit dict: Dictionary): Unit = dict.add(a)
    def transform(a: A): B = f(a)
  }
}

case class Expression()
object Expression {
  implicit val transformer = Transformer.instance[ast.Expression, Expression] { expr =>
    Expression()
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
  implicit val attributeNameTransformer = Transformer.instance[ast.AttributeName, universe.TermName] {
    case ast.SimpleAttributeName(name) => TermName(name.head.toLower +: name.tail)
  }
  implicit val boundsTransformer: Transformer[ast.Bounds, universe.Tree] = Transformer.instance {
    case ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Unknown) =>
      tq"""NonEmpty"""
    case ast.Bounds(ast.IntegerLiteral(min), ast.IntegerLiteral(max)) =>
      tq"""And[MinSize, MaxSize]"""
    case ast.Bounds(ast.IntegerLiteral(min), ast.BuiltInConstant.Unknown) =>
      tq"""MinSize"""
    /*case ast.Bounds(ast.IntegerLiteral(min), ast.BuiltInConstant.Unknown) =>
q"""MinSize[W.`$min`.T]"""*/
    //    case ast.Bounds(ast.IntegerLiteral(min), ast.IntegerLiteral(max))     =>
  }
  def composeWithBounds(tpe: universe.Tree, boundsOpt: Option[ast.Bounds]) = boundsOpt match {
    case None => tpe
    case Some(bounds) =>
      val tpt = Transformer(bounds)
      tq"""$tpe Refined $tpt"""
  }
  implicit val rootTypeTransformer: Transformer[ast.RootType, universe.Tree] = Transformer.instance {
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
  //implicit val parameterType: Transformer[ast.ParameterType,universe.Tree] = aggregationTypeLevel
  implicit val attributeTransformer = Transformer.instance[ast.ExplicitAttribute, universe.Tree] {
    case ast.ExplicitAttribute(attributeName, optional, tpe) =>
      val defName = Transformer(attributeName)
      val tpt = Transformer(tpe)
      val optionalType = optional match {
        case false => tpt
        case true  => q"Option[$tpt]"
      }
      q"""def $defName : $optionalType"""
  }

  implicit val entityTransformer = Transformer.instance[ast.EntityDeclaration, universe.Tree] { entity =>
    val tname = TypeName(entity.name)
    val stats = entity.attributes.map(Transformer(_))
    if(entity.supertype.isDefined)
    q"""trait $tname {
      ..$stats
      }"""
    else
      q"""class $tname {
      ..$stats
      }"""
  }

  implicit val typeTransformer = Transformer.instance[ast.TypeDeclaration, universe.Tree] { tpe =>
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
      case e:ast.ConcreteType =>
        val tpt = Transformer(e)
        q"""type $name = $tpt"""
    }
  }

  implicit val schemaBodyTransformer = Transformer.instance[ast.SchemaBody, universe.Tree] {
    case e: ast.EntityDeclaration   => Transformer(e)
    case e: ast.FunctionDeclaration => q""" def test() : Unit = {} """
    case e: ast.RuleDeclaration     => q""" def test() : Unit = {} """
    case e: ast.TypeDeclaration     => Transformer(e)
  }
}