package com.iz2use.express.generator

import com.iz2use.express.p11.ast
import scala.annotation.tailrec

trait Dictionary {
  def add[A](value: A): Unit
}
final case class TransformerContext(private val rootPackage: String = "") {
  //private var names: Map[String, ast.SchemaBody] = Map.empty
  private var inheritingTraits: Map[String, Seq[String]] = Map.empty
  private var packageStack: List[Seq[String]] = rootPackage match {
    case "" => List()
    case r  => List(r.split('.'))
  }
  final def addInheritTraitTo(entity: String, parent: String): Unit = {
    val v = inheritingTraits.getOrElse(entity, Nil)
    inheritingTraits = inheritingTraits + (entity -> (parent +: v))
  }
  final def inheritingTraitsOf(entity: String) = inheritingTraits.getOrElse(entity, Nil)
  final def subtypeOf(entity: String): Seq[String] = inheritingTraits.collect({
    case (subtype, parents) if parents.contains(entity) => subtype
  }).toSeq
  private var currentPackage: List[String] = packageStack.reverse.flatten
  final def withPackage[A](name: String)(f: => A): A = {
    val oldCurrentPackage = currentPackage
    packageStack = name.split('.') :: packageStack
    val newCurrentPackage = packageStack.reverse.flatten
    currentPackage = newCurrentPackage
    val r = f
    packageStack = packageStack.tail
    currentPackage = oldCurrentPackage
    r
  }
  @tailrec
  private final def loopOnContext[A](rest: List[Map[String, Any]], f: Map[String, Any] => Option[A]): Option[A] = rest match {
    case Nil => None
    case head :: tail =>
      f(head) match {
        case None => loopOnContext(tail, f)
        case some => some
      }
  }
  final def find(name: String): Option[Any] =
    loopOnContext(stack, _.get(name))

  final def targetPackage = currentPackage
  private var stack: List[Map[String, Any]] = List(Map.empty)
  final def register[A](key: String, value: A): Unit = {
    if (key.nonEmpty) {
      val newMap = stack.head + (key -> value)
      stack = newMap :: stack.tail
    }
  }
  final def getExactName(name: String): String = {
    loopOnContext(stack, _.find(_._1 equalsIgnoreCase name))
      .fold(name)(_._1)
  }
  final def pushContext: Unit = stack = Map.empty[String, Any] :: stack
  final def popContext: Unit = stack = stack.tail
}
object TransformerContext {
}
abstract class Transformer[-A, B] {
  def addToDictionary(a: A)(implicit context: TransformerContext): Unit
  final def transform(a: A)(implicit context: TransformerContext): B = {
    context.pushContext
    val r = impl(a)
    context.popContext
    r
  }
  def impl(a: A)(implicit context: TransformerContext): B
}

object Transformer {
  def addToDictionary[A, B](a: A)(implicit trsf: Transformer[A, B], context: TransformerContext): Unit = trsf.addToDictionary(a)
  def apply[A, B](a: A)(implicit trsf: Transformer[A, B], context: TransformerContext): B = trsf.transform(a)
  def instance[A, B](f: TransformerContext => A => B) = new Transformer[A, B] {
    def addToDictionary(a: A)(implicit context: TransformerContext): Unit = {}
    override def impl(a: A)(implicit context: TransformerContext): B = f(context)(a)
  }
  def typeInstance[A, B](returnName: A => String)(f: TransformerContext => A => B) = new Transformer[A, B] {
    def addToDictionary(a: A)(implicit context: TransformerContext): Unit = context.register(returnName(a), a)
    override def impl(a: A)(implicit context: TransformerContext): B = f(context)(a)
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
    q"import cats.syntax.functor._",
    q"import com.iz2use.express.syntax._",
    q"import com.iz2use.express.p21._",
    q"import com.iz2use.express.p21.generic.decoder._",
    q"import com.iz2use.express.p21.generic.encoder._",
    q"import eu.timepit.refined._",
    q"import eu.timepit.refined.api._",
    q"import eu.timepit.refined.boolean._",
    q"import eu.timepit.refined.collection._",
    q"import eu.timepit.refined.generic._",
    q"import eu.timepit.refined.numeric._",
    q"import eu.timepit.refined.string._",
    q"import shapeless.{ HList, ::, HNil }")

  implicit class RichString(s: String) {
    def lowerFirst = s.head.toLower +: s.tail
  }
  implicit class RichListString(s: List[String]) {
    def asPackage = s match {
      case head :: second :: tail => tail.foldLeft[universe.RefTree](q"""${TermName(head)}.${TermName(second)}""")((acc, v) => q"""$acc.${TermName(v)}""")
    }
  }

  implicit def seqTransformer[A, B](implicit trsf: Transformer[A, B]): Transformer[Seq[A], Seq[B]] = Transformer.instance { implicit ctx => (in: Seq[A]) =>
    in.map(trsf.impl(_))
  }

  implicit def optionTransformer[A, B](implicit trsf: Transformer[A, B]): Transformer[Option[A], Option[B]] = Transformer.instance { implicit ctx => (in: Option[A]) =>
    in.map(trsf.impl(_))
  }

  implicit val boundsTransformer: Transformer[ast.Bounds, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case ast.Bounds(ast.IntegerLiteral("1"), ast.BuiltInConstant.Unknown) =>
        tq"""NonEmpty"""
      case ast.Bounds(ast.IntegerLiteral(min), ast.IntegerLiteral(max)) =>
        if (min == max)
          tq"""Size[Equal[W.${TermName(s"`$min`")}.T]]"""
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
    case Some(Transformed(tpt)) =>
      tq"""$tpe Refined $tpt"""
  }

  def referenceToEntity(wanted: String)(implicit ctx: TransformerContext): universe.Tree = {
    val name = ctx.getExactName(wanted)
    def tpe(suffix: String = "") = Ident(TermName(name + suffix))
    ctx.find(name) match {
      case Some(a: ast.EntityDeclaration) =>
        val target = tpe()
        tq"""$target"""
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
      case ast.ArrayType(boundsOpt, optionalAllowed, uniqueOnly, Transformed(tpt)) =>
        composeWithBounds(tq"""Array[$tpt]""", boundsOpt)
      case ast.BagType(boundsOpt, Transformed(tpt)) =>
        composeWithBounds(tq"""Seq[$tpt]""", boundsOpt)
      case ast.BinaryType(width) =>
        tq"""Binary"""
      case ast.BooleanType =>
        tq"""Boolean"""
      case ast.GenericType(Some(label)) =>
        Ident(TermName(label))
      case ast.IntegerType =>
        tq"""Int"""
      case ast.ListType(boundsOpt, unique, Transformed(tpt)) =>
        composeWithBounds(tq"""List[$tpt]""", boundsOpt)
      case ast.LogicalType =>
        tq"""Logical"""
      case ast.NumberType =>
        tq"""Double"""
      case ast.RealType(width) =>
        tq"""Double"""
      case ast.SetType(boundsOpt, Transformed(tpt)) =>
        composeWithBounds(tq"""Set[$tpt]""", boundsOpt)
      case ast.StringType(maxLength) =>
        maxLength match {
          case Some(ast.Width(ast.IntegerLiteral(length), fixed)) =>
            val size = tq"""W.${TermName(s"`$length`")}.T"""
            fixed match {
              case true  => tq"""String Refined Size[Equal[$size]]"""
              case false => tq"""String Refined MaxSize[$size]"""
            }
          case None => tq"""String"""
        }
      case ast.UserDefinedEntity(tpe)       => referenceToEntity(tpe)
      case ast.UserDefinedEntityOrType(tpe) => referenceToEntity(tpe)
      case ast.UserDefinedType(tpe)         => referenceToEntity(tpe)
    }
  }

  private final def getAttributeName(name: ast.AttributeName) = name match {
    case ast.SimpleAttributeName(name)         => TermName(name.lowerFirst)
    case ast.RedeclaredAttribute(source, None) => TermName(source.attribute.lowerFirst)
  }

  object Attribute {
    sealed trait AttributeConversion[A, B] {
      type Repr = B
      def apply(attribute: A)(implicit context: TransformerContext): Option[B]
    }

    implicit final val explicitAttributeConversion = new AttributeConversion[ast.ExplicitAttribute, (universe.TermName, universe.Tree, Option[universe.Tree])] {
      def apply(attribute: ast.ExplicitAttribute)(implicit context: TransformerContext): Option[Repr] = {
        val Transformed(tpt) = attribute.tpe
        val (tptOpt, default) = attribute.optional match {
          case false => (tpt, None)
          case true  => (tq"Option[$tpt]", Some(q"""None"""))
        }
        val name = getAttributeName(attribute.names)
        Some((name, tptOpt, default))
      }
    }
    implicit final val derivedAttributeConversion = new AttributeConversion[ast.DerivedAttribute, (universe.TermName, universe.Tree, universe.Tree)] {
      def apply(attribute: ast.DerivedAttribute)(implicit context: TransformerContext): Option[Repr] = {
        val Transformed(tpt) = attribute.tpe
        val name = getAttributeName(attribute.names)
        val Transformed(expr) = attribute.value
        Some((name, tpt, expr))
      }
    }
    implicit final val tuple3AttributeConversion = new AttributeConversion[(ast.AttributeName, Option[Boolean], ast.ParameterType), (universe.TermName, universe.Tree)] {
      def apply(attribute: (ast.AttributeName, Option[Boolean], ast.ParameterType))(implicit context: TransformerContext): Option[Repr] = {
        val tpt = Transformer(attribute._3)
        val tptOpt = attribute._2 match {
          case Some(true) => tq"Option[$tpt]"
          case _          => tpt
        }
        val name = getAttributeName(attribute._1)
        Some((name, tptOpt))
      }
    }
    def unapply[A, B](attribute: A)(implicit context: TransformerContext, conversion: AttributeConversion[A, B]): Option[B] = {
      conversion(attribute)
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

  def transformWhereRule(tname: universe.Tree, whereClause: Option[ast.WhereClause]): (Seq[universe.Tree], Seq[TypeName]) = {
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
        q"""implicit val $whereRuleName : Validate[$tname, $whereRuleType] = Validate.alwaysPassed(${TermName(whereRule)}())""")
      wrs = wrs :+ whereRuleType
    }
    (trees, wrs)
  }

  type IndexedAttribute = (Int, TermName, universe.Tree, Option[universe.Tree])

  def indexedAttributes(attributes: Seq[ast.ExplicitAttribute])(implicit context: TransformerContext): Option[Seq[IndexedAttribute]] = {
    @tailrec
    def loop(index: Int, acc: Seq[IndexedAttribute], rest: Seq[ast.ExplicitAttribute]): Seq[IndexedAttribute] = rest match {
      case Seq() => acc
      case Seq(Attribute(name, tpe, defaultOpt), tail @ _*) =>
        val value = (index, name, tpe, defaultOpt)
        loop(
          index + 1,
          acc :+ value,
          tail)
    }
    attributes match {
      case Seq() => None
      case _     => Some(loop(1, Nil, attributes))
    }
  }

  private final def getSubtypeEntities(entity: String)(implicit ctx: TransformerContext) = {
    @tailrec
    def loop(subtypes: Seq[String], rest: Seq[String]): Seq[String] = rest match {
      case Seq() => subtypes
      case Seq(head, tail @ _*) =>
        val r = ctx.subtypeOf(head)
        r match {
          case Seq(first, rest @ _*) => loop(head +: subtypes, rest ++ tail)
          case Seq()                 => loop(subtypes, tail)
        }
    }
    loop(Nil, Seq(entity))
  }

  private final def getParentEntities(entity: ast.EntityDeclaration)(implicit ctx: TransformerContext) = {
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
    loop(Nil, entity.subtype)
  }

  private final def entityAbstractClass(entity: ast.EntityDeclaration, tname: TypeName, parents: Seq[TermName], isTrait: Boolean)(implicit ctx: TransformerContext) = {
    val derivedAttributeDefinitions = entity.derivedAttributes.map {
      case Attribute(defName, tpe, expr) => q"""lazy val $defName : $tpe = $expr"""
    }
    val explicitAttributeDefinitions = entity.attributes.map {
      case Attribute(defName, optionalType, _) => q"""def $defName : $optionalType"""
    }
    val attributeDefinitions = explicitAttributeDefinitions ++ derivedAttributeDefinitions
    if (isTrait)
      q"""trait $tname extends ..$parents {..$attributeDefinitions}"""
    else
      /*if (attributeDefinitions.isEmpty)
              q"""class $tname extends ..$parents {..$attributeDefinitions}"""
            else*/
      q"""abstract class $tname extends ..$parents {..$attributeDefinitions}"""
  }

  def subtypeCodec(tname: TypeName, subtypes: Seq[String], supertypeCodec: Option[(universe.Tree, universe.Tree)] = None): (universe.Tree, universe.Tree) = {
    val (encoderCases, decoders) =
      (subtypes.map({ subtype =>
        (cq"""_: ${TypeName(subtype)} => ${TermName(subtype)}.encoder.asInstanceOf[Encoder[$tname]]""",
          q"""${TermName(subtype)}.decoder""")
      }) ++ supertypeCodec.map({
        case (enc, dec) =>
          (cq"""_ => $enc""",
            dec)
      })).unzip
    (q"""Encoder.flatMap { case ..$encoderCases}""",
      decoders match {
        case Seq(one) => q""" $one.asInstanceOf[Decoder[$tname]] """
        case lst      => lst.reduceRight[universe.Tree]((c, acc) => q"""$c | $acc""")
      })
    /*decoders.foldRight[universe.Tree](q"""Decoder.alwaysFailed(${entity.name})""")((c, acc) =>
            q"""$c | $acc"""*/
  }

  implicit val entityTransformer = Transformer.typeInstance[ast.EntityDeclaration, Seq[universe.Tree]](_.name) { implicit ctx =>
    { entity =>
      ctx.pushContext
      val tname = TypeName(entity.name)
      val isAbstract = entity.supertype match {
        case Some(ast.AbstractEntityDeclaration) => true
        case Some(ast.AbstractSupertypeDeclaration(Some(ast.SupertypeOneOf(_)))) => true
        case Some(ast.SupertypeRule(ast.SupertypeOneOf(_))) => false
        case Some(other) => throw new NotImplementedError(s"$other not supported for entity")
        case _ => false
      }
      val compName = TermName(entity.name)
      val (whereRules, whereTypes) = transformWhereRule(tq"""$tname""", entity.whereClause)
      val parents = entity.subtype.map(n => TermName(n)) ++ ctx.inheritingTraitsOf(entity.name).map(TermName(_)) :+ TermName("ExpressEntity")
      val entityTree = getParentEntities(entity) :+ entity
      val indexedAllAttributesOpt = indexedAttributes(entityTree.flatMap(_.attributes))
      val derivedAttributes =
        for {
          entity <- entityTree
          attr <- entity.derivedAttributes
          TermName(name) = getAttributeName(attr.names)
        } yield {
          ctx.register(name, attr)
          name
        }
      val indexedAllAttributesList = indexedAllAttributesOpt.toList
      val indexedAllAttributesFlatten = indexedAllAttributesList.flatten
      for { (_, TermName(name), tpe, _) <- indexedAllAttributesFlatten } ctx.register(name, tpe)
      val definition = entityAbstractClass(entity, tname, parents, isAbstract || indexedAllAttributesOpt.isEmpty)
      def applyDefinition: Seq[universe.Tree] = indexedAllAttributesList.flatMap { lst =>
        val (applyParams, temps, inits) = (for {
          (index, name, tpe, defaultOpt) <- lst
          TermName(nameStr) = name
          if !derivedAttributes.contains(nameStr)
        } yield {
          val temp = TermName(s"_$index")
          if (derivedAttributes.contains(nameStr)) {
            (q"""val $name: Unit = ()""",
              q"""""",
              q"""""")
          } else {
            val arg = defaultOpt match {
              case Some(value) => q"""val $name: $tpe = $value"""
              case None        => q"""val $name: $tpe"""
            }
            (arg,
              q"""val $temp = $name""",
              q"""val $name : $tpe = $temp""")
          }
        }).unzip3
        val productElements = for {
          (index, name, _, _) <- lst
        } yield cq"${index - 1} => $name"
        Seq(q"""  def apply(..$applyParams) : $tname = {
    ..$temps
    new $tname {
    ..$inits
    def hashCode(): Int = {
      scala.runtime.ScalaRunTime._hashCode(this)
    }
    def productArity: Int = ${applyParams.length}
    def productElement(i: Int) : Any = i match {
      case ..$productElements
      case _ => throw new java.lang.IndexOutOfBoundsException(i.toString())
    }
    }
  }""")
      }
      /**
       * selfCodec :
       * Encoder: StepObject(${entity.name}, Vector(
       *   Encoder[argTpe](arg))) || HList with HNil + ::
       *   Decoder: HList > Decoder
       */
      val repr = indexedAllAttributesList flatMap { lst =>
        val hlist = lst.foldRight[universe.Tree](tq"HNil") {
          case ((_, TermName(name), c, _), acc) =>
            derivedAttributes.contains(name) match {
              case true  => tq"Unit :: $acc"
              case false => tq"$c :: $acc"
            }
        }
        Seq(q"""type Repr = $hlist""")
      }
      def selfCodec: Option[(universe.Tree, universe.Tree)] = indexedAllAttributesOpt map { attrs =>
        val encodeArgs = attrs.foldRight[universe.Tree](q"HNil") {
          case ((_, name @ TermName(nameStr), _, _), acc) =>
            (derivedAttributes contains nameStr) match {
              case true  => q"""() :: $acc"""
              case false => q"""c.$name :: $acc"""
            }
        }
        //val unapplyArgs = allAttributes map { case (idx,name,_,_) => pq"""${TermName(s"_$idx")}""" }
        // 1. HList => Decode HList
        // 2. HList.
        val decodeArgs = for {
          (idx, TermName(name), _, _) <- attrs
          if !derivedAttributes.contains(name)
        } yield q"r.at(${idx - 1})"
        //val unapplyDecode = decodeArgs.foldRight[universe.Tree](q"HNil") { case (c, acc) => pq"""$c :: $acc""" }
        val name = q"""${entity.name}"""
        (q"""ReprObjectEncoder[Repr].contramap{ (c:$tname) => $encodeArgs }""",
          q"""ReprDecoder[Repr].map { r => $compName(..$decodeArgs) }""")
      }
      def codec = {
        val codecOpt = entity.supertype match {
          case Some(ast.AbstractEntityDeclaration) => Some((q"""???""", q"""???"""))
          case Some(ast.AbstractSupertypeDeclaration(Some(ast.SupertypeOneOf(UserDefinedEntities(subtypes @ _*))))) => Some(subtypeCodec(tname, subtypes))
          case Some(ast.SupertypeRule(ast.SupertypeOneOf(UserDefinedEntities(subtypes @ _*)))) => Some(subtypeCodec(tname, subtypes, selfCodec))
          case None => selfCodec
        }
        codecOpt.toList.flatMap({
          case (encoder, decoder) => Seq(
            q"""implicit val encoder : Encoder[$tname] = $encoder""",
            q"""implicit val decoder : Decoder[$tname] = $decoder""")
        })
      }
      val objectBody: Seq[universe.Tree] = whereRules ++ codec ++ (isAbstract match {
        case true  => Nil
        case false => applyDefinition
      }) ++ repr
      val objectDefinition = q"""object $compName { ..$objectBody }"""
      ctx.popContext
      defaultImports ++ Seq(definition, objectDefinition).filter(_.nonEmpty)
    }
  }

  object UserDefinedEntities {
    def unapplySeq(tpe: Seq[ast.SupertypeExpression]): Option[Seq[String]] = Some(tpe map {
      case ast.UserDefinedEntity(name) => name
    })
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

  object Transformed {
    def unapply[A, B](a: A)(implicit trsf: Transformer[A, B], ctx: TransformerContext): Option[B] = Some(trsf.transform(a))
  }

  object CollectionConvertableFunction {
    def unapply(f: ast.FunctionOrEntityConstructor)(implicit ctx: TransformerContext): Option[(String, universe.Tree => universe.Tree)] = f match {
      case ast.BuiltInFunction.HiIndex => Some(("lastIndexWhere", src => q"""$src.${TermName("size")}-1"""))
      case ast.BuiltInFunction.LoIndex => Some(("indexWhere", _ => q"0"))
      case ast.BuiltInFunction.SizeOf  => Some(("count", src => q"$src.size"))
      case _                           => None
    }
  }
  object ConvertableFunction {
    def unapply(f: ast.FunctionOrEntityConstructor)(implicit ctx: TransformerContext): Option[Option[Seq[ast.Expression]] => universe.Tree] = f match {
      case ast.BuiltInFunction.Nvl =>
        Some {
          case Some(Seq(Transformed(opt), Transformed(default))) =>
            q"""$opt.getOrElse($default)"""
        }
      case _ => None
    }
  }

  implicit val expressionTransformer: Transformer[ast.RootExpression, universe.Tree] = Transformer.instance { implicit ctx =>
    (_: ast.RootExpression) match {
      case ast.SingleOperation(Transformed(lhs), op, Transformed(rhs)) => op match {
        case ast.IN => q""" $rhs.contains($lhs) """
        case _ =>
          val operator = op match {
            case ast.LT => "<"
            case ast.GT => ">"
            case ast.LE => "<="
            case ast.GE => ">="
            case ast.NE => "!="
            case ast.EQ => "=="
            case ast.TN => "ne"
            case ast.TE => "eq"
          }
          q""" $lhs ${TermName(operator)} $rhs"""
      }
      case ast.MultipleOperation(Transformed(lhs), subops) if subops.forall(_._1== ast.OrElseOp) =>
        q""" null """
      case ast.MultipleOperation(Transformed(lhs), subops) =>
        subops.foldLeft[universe.Tree](lhs) {
          case (lhs, (op, Transformed(rhs))) =>
            val operator = op match {
              case ast.AdditionOp       => "+"
              case ast.SubtractionOp    => "-"
              case ast.MultiplicationOp => "*"
              case ast.DivisionOp       => "/"
              case ast.ModuloOp         => "%"
              case ast.AndOp            => "&&"
              case ast.OrElseOp         => "||"
              case ast.OrOp             => "|"
            }
            q""" $lhs ${TermName(operator)} $rhs"""
        }
      case ast.AggregateInitializer(Transformed(items)) => q"""Seq(..$items)"""
      case e: ast.BuiltInConstant => e match {
        case ast.BuiltInConstant.Self    => q"""this"""
        case ast.BuiltInConstant.PI      => q"""Math.PI"""
        case ast.BuiltInConstant.E       => q"""Math.E"""
        case ast.BuiltInConstant.Unknown => q"""unknown"""
      }
      case ast.FunctionCallOrEntityConstructor(CollectionConvertableFunction((methodNameStr, convert)), Some(Seq(arg))) =>
        ctx.pushContext
        val r = arg match {
          case ast.QueryExpression(name, Transformed(source), condition) =>
            val vname = name.lowerFirst
            ctx.register(vname, source)
            val tpt = tq""
            val param = q"val ${TermName(vname)} : $tpt"
            val f = q"($param => ${Transformer(condition)})"
            q"$source.${TermName(methodNameStr)}($f)"
          case Transformed(source) =>
            convert(source)
        }
        ctx.popContext
        r
      case ast.QualifiedApply(Transformed(qualifiable), qualifiers) =>
        /*val origin = qualifiable match {
          case e:ast.BuiltInConstant => TermName(e.toString())
          case
        }*/
        //val source = parameters.foldLeft(qualifiable)((acc, args) => q"""$acc(..$args)""")
        qualifiers.foldLeft(qualifiable) { (source, qualifier) =>
          qualifier match {
            case ast.IndexQualifier(Transformed(index0), Transformed(index1Opt)) =>
              index1Opt.foldLeft(q"""$source($index0)""")((acc, index1) => q"""$acc($index1)""")
            case ast.AttributeQualifier(name) =>
              q"""$source.${TermName(name.lowerFirst)}"""
            case ast.GroupQualifier(name) => source match {
              case q"""$z.this""" => q"""this"""
              case _              => q"""$source \ ${TermName(name)}"""
            }
          }
        }
      case ast.UnaryOperation(ast.NotOp, Transformed(expr))                  => q""" !$expr """
      case ast.UnaryOperation(ast.NegativeOp, Transformed(expr))             => q""" -$expr """
      case ast.FunctionCallOrEntityConstructor(ConvertableFunction(f), args) => f(args)
      case ast.FunctionCallOrEntityConstructor(called, args) =>
        val target = called match {
          case f: ast.BuiltInFunction =>
            q""" ${TermName(f.toString().lowerFirst)} """
          case ast.UserDefinedFunctionOrEntityConstructor(name) =>
            q""" ${TermName(ctx.getExactName(name))} """
        }
        args match {
          case Some(args) =>
            q""" $target(..${args.map(Transformer(_))}) """
          case None =>
            target
        }
      case s: ast.Literal =>
        s match {
          case ast.LogicalLiteral(value) => value match {
            case Some(true)  => q"""True"""
            case Some(false) => q"""False"""
            case None        => q"""Unknown"""
          }
          case ast.IntegerLiteral(value) => q""" ${value.toInt} """
          case ast.RealLiteral(value)    => q""" ${value.toDouble} """
          case ast.StringLiteral(value)  => q""" $value """
          case ast.BinaryLiteral(value)  => q""" Binary() """
        }
      case ast.Repetition(Transformed(expr), Transformed(count)) =>
        q""" $expr * $count """
      //case _ => q""" null """
    }
  }

  implicit val functionOrEntityConstructorTransformer: Transformer[ast.FunctionOrEntityConstructor, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case e: ast.FunctionCallOrEntityConstructor           => expressionTransformer.impl(e)
      case ast.UserDefinedFunctionOrEntityConstructor(name) => q"""${TermName(ctx.getExactName(name))}"""
    }
  }

  /*implicit val logicalExpressionTransformer: Transformer[ast.LogicalExpression, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case e: ast.Expression => expressionTransformer.transform(e)
    }
  }*/

  /*  implicit val numericExpressionTransformer: Transformer[ast.NumericExpression, universe.Tree]=Transformer.instance {implicit ctx =>
    {
      case e:ast.Expression => expressionTransformer.transform(e)
    }
  }*/

  implicit val statementTransformer: Transformer[ast.Statement, universe.Tree] = Transformer.instance { implicit ctx =>
    {
      case ast.IfStatement(Transformed(condition), Transformed(ifPass), Transformed(ifFailed)) =>
        q"""if($condition) {
          ..$ifPass
        }else{
          ..$ifFailed
        }"""
      case ast.ReturnStatement(expr) =>
        expr match {
          case None                    => q"""return """
          case Some(Transformed(expr)) => q"""return $expr"""
        }
      case ast.AssignmentStatement(target, Nil, Transformed(expr)) =>
        q""" ${TermName(ctx.getExactName(target))} = $expr"""
      /*case ast.CaseStatement(Transformed(expr), Transformed(cases), Transformed(otherwise)) =>
        val other = otherwise.map { v => cq"""_ => $v""" }
        q""" $expr match {
..${cases ++ other}
}"""*/
      case ast.CompoundStatement(body) =>
        ctx.pushContext
        val r = Transformer(body)
        ctx.popContext
        q""" { ..$r } """
      case r =>
        q""" ${r.toString()}  """
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
        case ast.Parameter(name, Transformed(targetTpe)) =>
          val argName = TermName(name.lowerFirst)
          ctx.register(name.lowerFirst, targetTpe)
          q"""val $argName : $targetTpe"""
      })
      val head: Seq[universe.Tree] = Nil //function.head.map(Transformer(_)(algorithmHeadTransformer, ctx))
      //val body = function.body.map(Transformer(_))
      val body = Seq(q"""null.asInstanceOf[$tpt]""")
      ctx.popContext
      defaultImports :+
        q"""object $name extends ExpressFunction {
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

        val packageObjectDefinition = q"""package object ${TermName(name)} extends CommonIfcBase {
  def registerEntity[A](implicit encoder: Encoder[A], decoder: Decoder[A]) : Unit = {
  }
  ..$types
  ..$rules
  ..$register
}
"""

        val mainDeclaration = ((ctx.targetPackage :+ "Apackage").mkString("."), packageParent.mkString("."), defaultImports ++ otherImports :+ packageObjectDefinition)

        val target = ctx.targetPackage.mkString(".")
        val independentDeclarations = schema.body.collect {
          case e @ ast.EntityDeclaration(name, _, _, _, _, _, _, _) => ((ctx.targetPackage :+ name).mkString("."), target, entityTransformer.transform(e))
          case e @ ast.FunctionDeclaration(name, _, _, __, _)       => ((ctx.targetPackage :+ name).mkString("."), target, functionTransformer.transform(e))
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
          entities.foreach { e => context.addInheritTraitTo(e, a.name) }
        case _ =>
      }
      context.register(a.name, a)
    }
    private def getSubtypeAttributes(entityName: String)(implicit context: TransformerContext): Seq[ast.Attribute] = {
      /*val subtypes = context.subtypeOf(entityName)
      subtypes.map(getSubtypeAttributes(_))*/
      Nil
    }
    def impl(tpe: ast.TypeDeclaration)(implicit context: TransformerContext): Seq[universe.Tree] = {
      val name = TypeName(tpe.name)
      val cname = TermName(tpe.name)
      tpe.underlyingType match {
        case ast.SelectType(extensible, from) =>
          from match {
            case Some(Left(items)) =>
              val allFields = for {
                entityName <- items
              } yield {
                val fields = for {
                  mainEntity <- context.find(entityName).collect({
                    case e: ast.EntityDeclaration => e
                  }).toSeq
                  entity <- getParentEntities(mainEntity) :+ mainEntity
                  attribute <- entity.attributes ++ entity.derivedAttributes ++ getSubtypeAttributes(entityName)
                } yield (attribute match {
                  case ast.ExplicitAttribute(name, opt, tpe) =>
                    (name, Some(opt), tpe)
                  case ast.DerivedAttribute(name, tpe, _) =>
                    (name, None, tpe)
                })
                fields.distinct
              }
              val commonFields = allFields.reduce(_ intersect _).map {
                case Attribute((name, tpe)) =>
                  q"""def $name: $tpe"""
              }
              //val tpt = items.foldRight[universe.Tree](tq"CNil")((c, acc) => tq"${TypeName(c)} :+: $acc")
              val (encoder, decoder) = subtypeCodec(name, items, None)
              Seq(
                q"""trait $name {
  ..$commonFields
}""",
                q"""object $cname {
  implicit val encoder: Encoder[$name] = $encoder
  implicit val decoder: Decoder[$name] = $decoder.asInstanceOf[Decoder[$name]]
}""")
            case _ =>
              throw new NotImplementedError(s"SELECT BASED ON not supported (${tpe.name})")
          }
        case ast.EnumerationType(extensible, items) =>
          val (enumDefs, decodeCases, encodeCases) = items match {
            case None => (Nil, Nil, Nil)
            case Some(Left(items)) =>
              items.map({ item =>
                val entry = TermName(item.name)
                (q"""case object $entry extends $name""",
                  cq""" ${q"${item.name}"} => $cname.$entry """,
                  cq""" $cname.$entry => ${q"${item.name}"} """)
              }).unzip3
            case _ => throw new NotImplementedError(s"ENUMERATION BASED ON not supported (${tpe.name})")
          }
          val tpt = tq"""Enumeration"""
          val clsDefs: Seq[universe.Tree] = Nil
          val enum = q"""object $cname {
..$enumDefs
  implicit val encoder: Encoder[$name] = Encoder.encodeLiteral.contramap {
  case ..$encodeCases
  }
  implicit val decoder: Decoder[$name] = Decoder.decodeLiteral.collect {
  case ..$decodeCases
  }
}"""
          extensible match {
            case true => Seq(q"""abstract class $name extends ExpressEnumeration {
..$clsDefs
}""", enum)
            case false => Seq(q"""sealed abstract class $name extends ExpressEnumeration{
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
          val parents = context.inheritingTraitsOf(tpe.name).map(TypeName(_)) :+ TermName("ExpressType")
          // class A(val t: Double)
          // class B extends A
          //Seq(q"""class $name(val value: $tpt) extends ..$parents""")
          val (whereRules, whereTypes) = transformWhereRule(tpt, tpe.whereClause)
          val target = whereTypes.map(t => tq"""$cname.$t""") match {
            case Nil => tpt
            case Seq(one) =>
              tq"""$tpt Refined $one"""
            case Seq(one, two, rest @ _*) =>
              val refined = rest.foldLeft(tq"""$one And $two""")((acc, c) => tq"""$acc And $c""")
              tq"""$tpt Refined $refined"""
          }

          val (underlying, fromUnderlying, toUnderlying) = target match {
            case tq"Refined[..$targs]" => (targs.head, q"""$cname(value)""", q"""value.value.value""")
            case v                     => (v, q"""$cname(value)""", q"""value.value""")
          }
          Seq(
            //q"""type $name = $target""",
            q"""case class $name(value: $target) extends ..$parents""",
            q"""object $cname {
..$whereRules
implicit val encoder : Encoder[$name] = Encoder[$target].contramap[$name](_.value)
implicit val decoder : Decoder[$name] = Decoder[$target].map($cname(_))
implicit def ${TermName(s"to${tpe.name}")}(value: $underlying): $name = $fromUnderlying
implicit def ${TermName(s"from${tpe.name}")}(value: $name): $underlying = $toUnderlying}""")
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
    def impl(a: Any)(implicit context: TransformerContext): Seq[universe.Tree] = Nil
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

    def impl(a: ast.SchemaBody)(implicit context: TransformerContext): Seq[universe.Tree] = a match {
      case e: ast.EntityDeclaration   => Transformer(e)
      case e: ast.FunctionDeclaration => Transformer(e)(emptyTransformer(e.name), context)
      case e: ast.RuleDeclaration     => Nil
      case e: ast.TypeDeclaration     => Transformer(e)
    }
  }
}
