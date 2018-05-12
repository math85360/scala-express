package com.iz2use.express

import com.iz2use.express.p21._
import eu.timepit.refined._
import eu.timepit.refined.boolean._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import java.util.UUID
import java.util.Base64
import shapeless.{ Coproduct, :+:, CNil }
import shapeless.ops.coproduct.Inject
import shapeless.ops.coproduct.RuntimeInject


package object ifc {
  type IfcLengthMeasure = Double
  type IfcNonNegativeLengthMeasure = IfcLengthMeasure Refined NonNegative
  type IfcPositiveLengthMeasure = IfcLengthMeasure Refined Positive
  type IfcText = String
  type IfcAxis2Placement = /*SelectOf[*/ IfcAxis2Placement2D :+: IfcAxis2Placement3D :+: CNil /*]*/
  type NonEmptySingleLine = NonEmpty And MaxSize[W.`255`.T]
  type SizeBetween2And3 = MinSize[W.`2`.T] And MaxSize[W.`3`.T]
  type SizeBetween1And3 = MinSize[W.`1`.T] And MaxSize[W.`3`.T]
  type IfcReal = Double :+: CNil
  type SingleLineString = String Refined NonEmptySingleLine
  type IfcLabel = SingleLineString
  type IfcInteger = Int
  /*type IfcLogical = Boolean :+: Null :+: CNil
  val True = true
  val False = false
  val Unknown = null*/
  sealed trait IfcLogical
  case object True extends IfcLogical
  case object False extends IfcLogical
  case object Unknown extends IfcLogical
  object IfcLogical {
    implicit val encoder: Encoder[IfcLogical] = Encoder.instance {
      case True    => Step.True
      case False   => Step.False
      case Unknown => Step.Unknown
    }
  }
  type NonEmptyWithFixedSize22 = NonEmpty And MatchesRegex[W.`"[0-9A-Za-z_$]{22}"`.T]
  /*And
  Size[W.`22`.T]*/
  //type IfcGloballyUniqueId = String Refined NonEmptyWithFixedSize22
  /**
   * Encoder/Decoder need to ensure is base64 on 22 chars
   */
  type IfcGloballyUniqueId = String Refined NonEmptyWithFixedSize22
  //= UUID
  object IfcGloballyUniqueId {
    private[this] val base64 = "ABCDEFGIHJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$"
    private[this] val base64mask = 0x3F

    def next(implicit db: DatabaseIfc): IfcGloballyUniqueId = {
      def c(l: Long, pos: Int): Char = {
        base64.charAt((base64mask & ((l.>>(pos * 6)).toInt)))
      }
      val uuid = UUID.randomUUID()
      val msb = uuid.getMostSignificantBits
      val lsb = uuid.getLeastSignificantBits
      val charArray = Array[Char](
        c(msb, 10), c(msb, 9), c(msb, 8), c(msb, 7), c(msb, 6), c(msb, 5),
        c(msb, 4), c(msb, 3), c(msb, 2), c(msb, 1), c(msb, 0),
        c(lsb, 10), c(lsb, 9), c(lsb, 8), c(lsb, 7), c(lsb, 6), c(lsb, 5),
        c(lsb, 4), c(lsb, 3), c(lsb, 2), c(lsb, 1), c(lsb, 0)
      )
      Refined.unsafeApply(String.valueOf(charArray))
    }

    //implicit val encoder = Encoder[String Refined NonEmptyWithFixedSize22].contramap[IfcGloballyUniqueId](_.value)
    //UUID.randomUUID()
  }
  case class IfcTimeStamp(val value: Int) extends AnyVal
  type IfcIdentifier = SingleLineString

  /*implicit def toRefined[A, P] (value: A)(
      implicit v: eu.timepit.refined.api.Validate[A, P]) : A Refined P = refineV[*/
  //implicit def toRefined[A, P]: A Refined P = refineMV[P]
  implicit def toRefined[A, P](value: A)(
    implicit
    v: eu.timepit.refined.api.Validate[A, P]
  ): A Refined P =
    api.Refined.unsafeApply[A, P](value)
  /*refineV[P](value) match {
      case Left(err) => throw new Error(err)
      case Right(v)  => v
    }*/
    implicit def toOption[A](value: A): Option[A] = Option(value)
    
  implicit def toOptionRefined[A, P](value: A)(
    implicit
    v: eu.timepit.refined.api.Validate[A, P]
  ): Option[A Refined P] = Some(api.Refined.unsafeApply[A, P](value))
  /*refineV[P](value) match {
      case Left(_)  => None
      case Right(v) => Some(v)
    }*/

  //implicit def toCoproduct[C <: Coproduct, T](value: T)(implicit inj: Inject[C, T]): C = inj(value)

  trait RefTo[+A]
  case class InsideRefTo[A](id: Int) extends RefTo[A]
  case class RefToDerived[A]() extends RefTo[A]
  object RefTo {

    implicit val order = new Ordering[RefTo[_]] {
      val max = Int.MaxValue - 1
      val ordering = implicitly[Ordering[Int]]
      def compare(x: RefTo[_], y: RefTo[_]): Int =
        x match {
          case InsideRefTo(a) =>
            y match {
              case InsideRefTo(b) => ordering.compare(a,b)
              case _              => a
            }
          case _ =>
            y match {
              case InsideRefTo(b) => b
              case _              => 0
            }
        }

    }
    implicit def encoder[A] = Encoder.instance[RefTo[A]] {
      case InsideRefTo(id) => StepReference(id)
      case _               => Step.Unknown
    }
    implicit def toRef[A](a: A)(implicit db: DatabaseIfc): RefTo[A] =
      db.insert(a)
  }
  implicit def toOptionRefTo[A](value: A)(implicit db: DatabaseIfc): Option[RefTo[A]] = Some(RefTo.toRef(value))

  implicit def fromEnumeration[A <: Enumeration](value: A#Value): A = value

  case class DatabaseIfc() {
    private var refToItem = scala.collection.immutable.TreeMap[RefTo[_], Any]()
    private var itemToRef = Map[Any, RefTo[_]]()
    private var nextRef: Int = 1

    def counter = nextRef

    def insert[A](a: A): RefTo[A] =
      itemToRef.get(a) match {
        case Some(v: RefTo[A]) => v
        case _ =>
          val v = nextRef
          nextRef += 1
          val r = InsideRefTo[A](v)
          refToItem += r -> a
          itemToRef += a -> r
          r
      }

    def update[A](ref: RefTo[A], newValue: A):Unit= {
      val oldValue = refToItem(ref)
      refToItem = refToItem.updated(ref, newValue)
      itemToRef = (itemToRef - oldValue) + (newValue -> ref)
    }

    def update[A](oldValue: A, newValue: A):Unit= {
      val ref = itemToRef(oldValue)
      refToItem = refToItem.updated(ref, newValue)
      itemToRef = (itemToRef - oldValue) + (newValue -> ref)
    }

    def remove[A](ref: RefTo[A]) :Unit={
      val value = refToItem(ref)
      refToItem = refToItem - ref
      itemToRef = (itemToRef - value)
    }

    def remove[A](value: A) :Unit={
      val ref = itemToRef(value)
      refToItem = refToItem - ref
      itemToRef = (itemToRef - value)
    }

    def apply[A](a: RefTo[A]): Option[A] =
      refToItem.get(a) match {
        case Some(r: A) => r
        case _          => None
      }

    override def toString() = {
      s"DatabaseIfc($counter, " +
        refToItem.iterator.map(v => s"#${v._1} = ${v._2}").mkString("\n  ", "\n  ", "\n") + ")"
    }

  }
}