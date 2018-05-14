package com.iz2use.express.generator

import com.iz2use.express.p11.ast
import scala.annotation.tailrec

trait Dictionary {
  def add[A](value: A): Unit
}
case class TransformerContext(private val rootPackage: String = "") {
  //private var names: Map[String, ast.SchemaBody] = Map.empty
  private var inheritingTraits: Map[String, Seq[String]] = Map.empty
  private var packageStack: List[Seq[String]] = rootPackage match {
    case "" => List()
    case r  => List(r.split('.'))
  }
  def addInheritTraitTo(entity: String, parent: String): Unit = {
    val v = inheritingTraits.getOrElse(entity, Nil)
    inheritingTraits = inheritingTraits + (entity -> (parent +: v))
  }
  def inheritingTraitsOf(entity: String) = inheritingTraits.getOrElse(entity, Nil)
  private var currentPackage: List[String] = packageStack.reverse.flatten
  def withPackage[A](name: String)(f: => A): A = {
    val oldCurrentPackage = currentPackage
    packageStack = name.split('.') :: packageStack
    val newCurrentPackage = packageStack.reverse.flatten
    currentPackage = newCurrentPackage
    val r = f
    packageStack = packageStack.tail
    currentPackage = oldCurrentPackage
    r
  }
  def find(name: String): Option[Any] = {
    @tailrec
    def loop(rest: List[Map[String, Any]]): Option[Any] = rest match {
      case Nil => None
      case head :: tail => head.get(name) match {
        case None  => loop(tail)
        case found => found
      }
    }
    loop(stack)
  }
  def targetPackage = currentPackage
  private var stack: List[Map[String, Any]] = List(Map.empty)
  def register[A](key: String, value: A): Unit = {
    if (key.nonEmpty) {
      val newMap = stack.head + (key -> value)
      stack = newMap :: stack.tail
    }
  }
  def getExactName(name: String): String = {
    @tailrec
    def loop(rest: List[Map[String, Any]]): String = rest match {
      case Nil => name
      case head :: tail =>
        head.find(_._1.equalsIgnoreCase(name)) match {
          case Some((found, _)) => found
          case None             => loop(tail)
        }
    }
    loop(stack)
  }
  def pushContext: Unit = stack = Map.empty[String, Any] :: stack
  def popContext: Unit = stack = stack.tail
}
object TransformerContext {
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
  val defaultImports = Seq(
    q"import com.iz2use.express.syntax._",
    q"import com.iz2use.express.p21._",
    q"import eu.timepit.refined._",
    q"import eu.timepit.refined.api._",
    q"import eu.timepit.refined.boolean._",
    q"import eu.timepit.refined.collection._",
    q"import eu.timepit.refined.numeric._",
    q"import eu.timepit.refined.string._")

  implicit class RichString(s: String) {
    def lowerFirst = s.head.toLower +: s.tail
  }
  implicit class RichListString(s: List[String]) {
    def asPackage = s match {
      case head :: second :: tail => tail.foldLeft[universe.RefTree](q"""${TermName(head)}.${TermName(second)}""")((acc, v) => q"""$acc.${TermName(v)}""")
    }
  }

  implicit val attributeNameTransformer: Transformer[ast.AttributeName, universe.TermName] = Transformer.instance { implicit ctx =>
    {
      case ast.SimpleAttributeName(name) => TermName(name.lowerFirst)
    }
  }

  implicit val boundsTransformer: Transformer[ast.Bounds, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Unknown) =>
        tq"""NonEmpty"""
      case ast.Bounds(ast.IntegerLiteral(min), ast.IntegerLiteral(max)) =>
        if (min == max)
          tq"""Size[W.${TermName(s"`$min`")}.T]"""
        else
          tq"""And[MinSize[W.${TermName(s"`$min`")}.T], MaxSize[W.${TermName(s"`$max`")}.T]]"""
      //case ast.Bounds(ast.IntegerLiteral(min), ast.BuiltInConstant.Unknown) =>
      //tq"""MinSize"""
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

  def referenceToEntity(name: String)(implicit ctx: TransformerContext): universe.Tree = {
    def tpe(suffix: String = "") = Ident(TermName(name + suffix))
    ctx.find(name) match {
      case Some(a: ast.EntityDeclaration) =>
        val target = a match {
          //case HasAbstract() => tpe("Abstract")
          case _ => tpe()
        }
        tq"""RefTo[$target]"""
      case _ =>
        tpe()
    }
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
        maxLength match {
          case Some(ast.Width(ast.IntegerLiteral(length), fixed)) =>
            val size = tq"""W.${TermName(s"`$length`")}.T"""
            fixed match {
              case true  => tq"""String Refined Size[$size]"""
              case false => tq"""String Refined MaxSize[$size]"""
            }
          case None => tq"""String"""
        }
      case ast.UserDefinedEntity(tpe)       => referenceToEntity(tpe)
      case ast.UserDefinedEntityOrType(tpe) => referenceToEntity(tpe)
      case ast.UserDefinedType(tpe)         => referenceToEntity(tpe)
    }
  }
  //implicit val parameterType: Transformer[ast.ParameterType,universe.Tree] = aggregationTypeLevel
  val attributeToDefTransformer: Transformer[ast.ExplicitAttribute, universe.Tree] = Transformer.instance { implicit ctx =>
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
  val attributeToValTransformer: Transformer[ast.ExplicitAttribute, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case ast.ExplicitAttribute(attributeName, optional, tpe) =>
        val defName = Transformer(attributeName)
        val tpt = Transformer(tpe)
        optional match {
          case false => q"val $defName : $tpt"
          case true  => q"val $defName : Option[$tpt] = None"
        }
    }
  }

  object HasAbstract {
    def unapply(entity: ast.EntityDeclaration): Boolean = entity.supertype match {
      case Some(ast.AbstractEntityDeclaration) => true
      case Some(ast.AbstractSupertypeDeclaration(Some(ast.SupertypeOneOf(_)))) => true
      case Some(ast.SupertypeRule(ast.SupertypeOneOf(_))) => true
      case Some(other) => throw new NotImplementedError(s"$other not supported for entity")
      case _ => false
    }
  }

  object HasConcrete {
    def unapply(entity: ast.EntityDeclaration): Boolean = entity.supertype match {
      case Some(ast.AbstractEntityDeclaration) => false
      case Some(ast.AbstractSupertypeDeclaration(Some(ast.SupertypeOneOf(_)))) => false
      case Some(ast.SupertypeRule(ast.SupertypeOneOf(_))) => true
      case Some(other) => throw new NotImplementedError(s"$other not supported for entity")
      case _ => true
    }
  }

  def transformWhereRule(tname: TypeName, whereClause: Option[ast.WhereClause]): (Seq[universe.Tree], Seq[TypeName]) = {
    var defaultNumber: Int = 0
    var trees: Seq[universe.Tree] = Nil
    var wrs: Seq[TypeName] = Nil
    for {
      v <- whereClause
      ast.DomainRule(nameOpt, expr) <- v.clauses
    } {
      val whereRule = nameOpt.getOrElse({
        defaultNumber += 1
        s"WhereRule$defaultNumber"
      })
      val whereRuleType = TypeName(whereRule)
      val whereRuleName = TermName(whereRule.lowerFirst)
      trees = trees ++ Seq(
        q"""final case class $whereRuleType()""",
        q"""val $whereRuleName : Validate[$tname, $whereRuleType] = Validate.alwaysPassed(${TermName(whereRule)}())""")
      wrs = wrs :+ whereRuleType
    }
    (trees, wrs)
  }

  implicit val entityTransformer = Transformer.typeInstance[ast.EntityDeclaration, Seq[universe.Tree]](_.name) { implicit ctx =>
    { entity =>
      val tname = TypeName(entity.name)
      val aname = tname
      val isAbstract = HasAbstract.unapply(entity)
      val cname = if (isAbstract) aname else tname
      val (whereRules, whereTypes) = transformWhereRule(cname, entity.whereClause)
      @tailrec
      def loop(supertypes: Seq[ast.EntityDeclaration], parents: Seq[String]): Seq[ast.EntityDeclaration] =
        parents match {
          case Seq() => supertypes
          case Seq(head, tail @ _*) =>
            val r = ctx.find(head)
            r match {
              case Some(e: ast.EntityDeclaration) => loop(e +: supertypes, e.subtype ++ tail)
              case None                           => loop(supertypes, tail)
            }
        }
      val parents = entity.subtype.map(n => TermName(n)) ++ ctx.inheritingTraitsOf(entity.name).map(TermName(_))
      val parentEntities = loop(Nil, entity.subtype)

      def getAttributes = {
        entity.attributes.map(attributeToDefTransformer.transform(_))
      }
      val allAttributes = parentEntities.flatMap(_.attributes) ++ entity.attributes
      def paramss = List(allAttributes.map(attributeToValTransformer.transform(_)))
      val definition = q"""class $tname extends ..$parents {
..${getAttributes}
}"""
      def applyParamss = List(allAttributes.map(attributeToValTransformer.transform(_)))
      val objectDefinition = q"""object ${TermName(entity.name)} {
def apply(...$applyParamss) : $tname = new $tname {
}
..$whereRules
}"""
      defaultImports ++ Seq(definition, objectDefinition).filter(_.nonEmpty)
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

  implicit val expressionTransformer: Transformer[ast.Expression, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case ast.SingleOperation(lhs, op, rhs) => op match {
        case ast.IN => q""" ${Transformer(rhs)}.contains(${Transformer(lhs)}) """
        case _      => q""" null """
      }
      //case ast.MultipleOperation(lhs, subops) =>
      case ast.UnaryOperation(ast.NotOp, expr) => q""" !${Transformer(expr)} """
      case ast.FunctionCallOrEntityConstructor(called, args) =>
        val target = called match {
          case f: ast.BuiltInFunction => q""" ${TermName(f.toString().lowerFirst)} """
          case ast.UserDefinedFunctionOrEntityConstructor(name) =>
            q""" ${TermName(ctx.getExactName(name))} """
        }
        args match {
          case Some(args) =>
            q""" $target(..${args.map(Transformer(_))}) """
          case None =>
            target
        }
      case _ => q""" null """
    }
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
      case ast.LocalDeclaration(_name, tpe, expr) =>
        val name = _name.lowerFirst
        ctx.register(name, tpe)

        expr match {
          case None       => q"""var ${TermName(name)} : ${Transformer(tpe)} =_"""
          case Some(expr) => q"""var ${TermName(name)} : ${Transformer(tpe)} = ${Transformer(expr)}"""
        }
    }
  }

  implicit val functionTransformer: Transformer[ast.FunctionDeclaration, Seq[universe.Tree]] = Transformer.typeInstance[ast.FunctionDeclaration, Seq[universe.Tree]](_.name) { implicit ctx =>
    { function =>
      ctx.pushContext
      val name = TermName(function.name)
      val tpt = Transformer(function.tpe)
      val genericTpts = function.parameters.collect({
        case ast.Parameter(_, HasGenericType(name)) =>
          name
      }).distinct.map({ name =>
        ctx.register(name, null)
        TypeDef(Modifiers(), TypeName(name), Nil, q"")
      })
      val paramss = function.parameters.map({
        case ast.Parameter(name, argTpe) =>
          val argName = TermName(name.lowerFirst)
          val targetTpe = Transformer(argTpe)
          ctx.register(name.lowerFirst, targetTpe)
          q"""val $argName : $targetTpe"""
      })
      val head = function.head.map(Transformer(_)(algorithmHeadTransformer, ctx))
      val body = function.body.map(Transformer(_))
      ctx.popContext
      defaultImports :+
        q"""object $name {
  def apply[..$genericTpts](..$paramss): $tpt = {
    ..$head
    ..$body
  }
}"""
    }
  }

  implicit val schemaTransformer: Transformer[ast.Schema, Seq[(String, String, Seq[universe.Tree])]] = Transformer.instance { implicit ctx =>
    { schema =>
      ctx.withPackage(schema.id.toLowerCase) {
        schema.body.foreach(Transformer.addToDictionary(_))

        //val targetPackage = ctx.targetPackage

        val (packageParent, name) = ctx.targetPackage match {
          case Seq(one) => (List("_root_"), one)
          case lst      => (lst.init, lst.last)
        }

        val types = schema.body flatMap {
          case e: ast.TypeDeclaration => Transformer(e)
          case _                      => Nil
        }

        val register = schema.body collect {
          case e: ast.EntityDeclaration => q"""registerEntity[${TypeName(e.name)}]"""
        }

        val rules = Seq[universe.Tree]()
        
        val otherImports = Seq(q"""import shapeless.{ Coproduct, :+:, CNil }""")
        
        val packageObjectDefinition = q"""package object ${TermName(name)} {
  def registerEntity[A](implicit encoder: Encoder[A], decoder: Decoder[A]) : Unit = {
  }
  ..$types
  ..$rules
  ..$register
}
"""

        val mainDeclaration = ((ctx.targetPackage :+ "package").mkString("."), packageParent.mkString("."), defaultImports ++ otherImports :+ packageObjectDefinition 
          )

        val target = ctx.targetPackage.mkString(".")
        val independentDeclarations = schema.body.collect {
          case e @ ast.EntityDeclaration(name, _, _, _, _, _, _, _) => ((ctx.targetPackage :+ name).mkString("."), target, Transformer(e))
          case e @ ast.FunctionDeclaration(name, _, _, __, _)       => ((ctx.targetPackage :+ name).mkString("."), target, Transformer(e))
        }

        mainDeclaration +: independentDeclarations
      }
    }
  }

  /**
   * Transformer[A, B] {
   * def addToDictionary(a: A)(implicit context: TransformerContext): Unit = context.register(returnName(a), a)
   * def transform(a: A)(implicit context: TransformerContext): B = {
   * context.pushContext
   * val r = f(context)(a)
   * context.popContext
   * r
   * }
   * }
   */
  implicit val typeTransformer = new Transformer[ast.TypeDeclaration, Seq[universe.Tree]] {
    def addToDictionary(a: ast.TypeDeclaration)(implicit context: TransformerContext): Unit = {
      a.underlyingType match {
        case ast.SelectType(extensible, Some(Left(entities))) =>
        //entities.foreach { e => context.addInheritTraitTo(e, a.name) }
        case _ =>
      }
      context.register(a.name, a)
    }
    def transform(tpe: ast.TypeDeclaration)(implicit context: TransformerContext): Seq[universe.Tree] = {
      val name = TypeName(tpe.name)
      tpe.underlyingType match {
        case ast.SelectType(extensible, from) =>
          val tpt = from match {
            case Some(Left(items)) => items.foldRight[universe.Tree](tq"CNil")((c, acc) => tq"${TypeName(c)} :+: $acc")
            case _                 => throw new NotImplementedError("SELECT BASED ON not supported (${tpe.name})")
          }
          Seq(q"""type $name = $tpt""")
        case ast.EnumerationType(extensible, items) =>
          val enumDefs = items match {
            case None => Nil
            case Some(Left(items)) => items map { item =>
              q"""case object ${TermName(item.name)} extends $name"""
            }
            case _ => throw new NotImplementedError("ENUMERATION BASED ON not supported (${tpe.name})")
          }
          val tpt = tq"""Enumeration"""
          q"""type $name = $tpt"""
          val clsDefs: Seq[universe.Tree] = Nil
          val enum = q"""object ${TermName(tpe.name)} {
..$enumDefs
}"""
          extensible match {
            case true => Seq(q"""abstract class $name extends scala.Product with scala.Serializable {
..$clsDefs
}""", enum)
            case false => Seq(q"""sealed abstract class $name extends scala.Product with scala.Serializable {
..$clsDefs 
}""", enum)
          }
        /*case e: ast.SimpleType =>
          val tpt = Transformer(e)
          val parents = context.inheritingTraitsOf(tpe.name).map(TypeName(_))
          Seq(q"""case class $name(val value: $tpt) extends ..$parents""")*/
        case e: ast.ConcreteType =>
          val tpt = Transformer(e)
          val cname = TermName(tpe.name)
          //val parents = context.inheritingTraitsOf(tpe.name).map(TypeName(_))
          // class A(val t: Double)
          // class B extends A
          //Seq(q"""class $name(val value: $tpt) extends ..$parents""")
          val (whereRules, whereTypes) = transformWhereRule(name, tpe.whereClause)
          val target = whereTypes.map(t => tq"""$cname.$t""") match {
            case Nil => tpt
            case Seq(one) =>
              tq"""$tpt Refined $one"""
            case Seq(one, two, rest @ _*) =>
              val refined = rest.foldLeft(tq"""$one And $two""")((acc, c) => tq"""$acc And $c""")
              tq"""$tpt Refined $refined"""
          }
          Seq(
            q"""type $name = $target""",
            q"""object $cname {
..$whereRules
}""")
      }
    }
  }

  /**
   * abstract class Transformer[-A, B] {
   * def addToDictionary(a: A)(implicit context: TransformerContext): Unit
   * def transform(a: A)(implicit context: TransformerContext): B
   * }
   */
  //def emptyTransformer[A](a: A)(pf: A => String) =
  class EmptyTransformer(name: String) extends Transformer[Any, Seq[universe.Tree]] {
    def addToDictionary(a: Any)(implicit context: TransformerContext): Unit = {
      context.register(name, a)
    }
    def transform(a: Any)(implicit context: TransformerContext): Seq[universe.Tree] = Nil
  }

  def emptyTransformer(name: String = "") = new EmptyTransformer(name)

  implicit val schemaBodyTransformer = new Transformer[ast.SchemaBody, Seq[universe.Tree]] {
    def addToDictionary(a: ast.SchemaBody)(implicit context: TransformerContext): Unit = a match {
      case e: ast.EntityDeclaration   => entityTransformer.addToDictionary(e)
      case e: ast.FunctionDeclaration => functionTransformer.addToDictionary(e)
      case e: ast.RuleDeclaration     =>
      case e: ast.TypeDeclaration     => typeTransformer.addToDictionary(e)
      case _                          =>
    }

    def transform(a: ast.SchemaBody)(implicit context: TransformerContext): Seq[universe.Tree] = a match {
      case e: ast.EntityDeclaration   => Transformer(e)
      case e: ast.FunctionDeclaration => Transformer(e)(emptyTransformer(e.name), context)
      case e: ast.RuleDeclaration     => Nil
      case e: ast.TypeDeclaration     => Transformer(e)
    }
  }
}