package com.iz2use.express.p21

import cats.{ MonadError, SemigroupK }
import cats.data.{ Validated }
import cats.instances.either.{ catsStdInstancesForEither, catsStdSemigroupKForEither }
import com.iz2use.express.syntax._
import eu.timepit.refined._
import eu.timepit.refined.api._
import scala.{ Left, Right, Either }
import scala.util.{ Try, Success, Failure }
import shapeless.{ ::, Generic, HList, Lazy, HNil, Coproduct, CNil, :+:, Inl, Inr }
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom

trait Decoder[A] extends Serializable { self =>
  def apply(c: HCursor): Decoder.Result[A]

  private[p21] def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] = apply(c) match {
    case Right(a) => Validated.valid(a)
    case Left(e)  => Validated.invalidNel(e)
  }

  def tryDecode(c: ACursor): Decoder.Result[A] = c match {
    case hc: HCursor => apply(hc)
    case _ => Left(
      DecodingFailure("Attempt to decode value on failed cursor", c.history))
  }

  def tryDecodeAccumulating(c: ACursor): AccumulatingDecoder.Result[A] = c match {
    case hc: HCursor => decodeAccumulating(hc)
    case _ => Validated.invalidNel(
      DecodingFailure("Attempt to decode value on failed cursor", c.history))
  }

  final def or[AA >: A](d: => Decoder[AA]): Decoder[AA] = new Decoder[AA] {
    final def apply(c: HCursor): Decoder.Result[AA] = self(c) match {
      case r @ Right(_) => r
      case Left(_)      => d(c)
    }
  }

  final def accumulating: AccumulatingDecoder[A] = AccumulatingDecoder.fromDecoder(self)

  def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    final def apply(c: HCursor): Decoder.Result[B] = tryDecode(c)
    override def tryDecode(c: ACursor): Decoder.Result[B] = self.tryDecode(c) match {
      case Right(a)    => Right(f(a))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
    }
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[B] =
      tryDecodeAccumulating(c)

    override final def tryDecodeAccumulating(c: ACursor): AccumulatingDecoder.Result[B] =
      self.tryDecodeAccumulating(c).map(f)
  }

  def collect[B](pf: PartialFunction[A, B]): Decoder[B] = new Decoder[B] {
    final def apply(c: HCursor): Decoder.Result[B] = tryDecode(c)
    override def tryDecode(c: ACursor): Decoder.Result[B] = self.tryDecode(c) match {
      case Right(a) =>
        if (pf.isDefinedAt(a)) Right(pf(a))
        else Left(DecodingFailure("MatchError", c.history))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
    }
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[B] =
      tryDecodeAccumulating(c)

    override final def tryDecodeAccumulating(c: ACursor): AccumulatingDecoder.Result[B] =
      self.tryDecodeAccumulating(c) match {
        case v @ Validated.Valid(a) =>
          Validated.condNel(
            pf.isDefinedAt(a),
            pf(a),
            DecodingFailure("Attempt to collect", c.history))
        case l @ Validated.Invalid(_) => l
      }
  }

  /*def orElse[B<:A, C>:B](other: Decoder[C]): Decoder[B] = apply(c) match {
    case Left(e) => other(c)
  }*/

  /*def tryDecodeAccumulating(c: Step): AccumulatingDecoder.Result[A] = f(c) match {
      case Right(v) => Validated.valid(v)
      case Left(e) => Validated.invalidNel(e)
    }*/

  /*final def map[B](f: A=>B):Decoder[B] = new Decoder[B] {
    final
  }*/
}

final object Decoder extends TupleDecoders with HListInstancesDecoder {

  type Result[A] = Either[DecodingFailure, A]

  final val resultInstance: MonadError[Result, DecodingFailure] = catsStdInstancesForEither[DecodingFailure]

  def apply[A](implicit instance: Decoder[A]) = instance

  def instance[A](f: HCursor => Result[A]): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor): Decoder.Result[A] = f(c)
  }
  def instancePf[A](name: String)(f: PartialFunction[Step, A]): Decoder[A] = instance { c =>
    f.isDefinedAt(c.value) match {
      case true  => Right(f(c.value))
      case false => Left(DecodingFailure(name, c.history))
    }
  }
  /*def decodeObject[A](f: StepObject => A): Decoder[A] = instance { c =>

  }*/
  //def decodeHList[L >: HList]
  def alwaysFailed[A](name: String): Decoder[A] = instance { c =>
    Left(DecodingFailure(name, c.history))
  }

  def const[A](a: A): Decoder[A] = new Decoder[A] {
    def apply(c: HCursor): Result[A] = Right(a)
    final override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      Validated.valid(a)
  }

  final val decodeLiteral: Decoder[String] = instancePf("Literal") {
    case StepLiteral(literal) => literal.stripPrefix(".").stripSuffix(".")
  }

  implicit final val decodeBoolean: Decoder[Boolean] = instancePf("Boolean") {
    case StepBoolean(boolean) => boolean
  }

  implicit final val decodeLogical: Decoder[Logical] = instancePf("Logical") {
    case StepBoolean(boolean) => boolean match {
      case true  => True
      case false => False
    }
    case StepUnknown => Unknown
  }

  implicit final val decodeLong: Decoder[Long] = instancePf("Long") {
    case StepLong(v) => v
  }
  implicit final val decodeInt: Decoder[Int] = instancePf("Int") {
    case StepInt(v) => v
  }
  implicit final val decodeDouble: Decoder[Double] = instancePf("Double") {
    case StepDouble(v) => v
  }
  implicit final val decodeFloat: Decoder[Float] = instancePf("Float") {
    case StepFloat(v) => v
  }

  implicit final val decodeString: Decoder[String] = instancePf("String") {
    case StepString(string) => string
  }

  implicit final def decodeRefTo[A]: Decoder[RefTo[A]] = instancePf("RefTo") {
    case StepReference(id) => RefTo(id)
  }

  implicit final def decodeArray[A](implicit decodeA: Decoder[A], cbf: CanBuildFrom[Nothing, A, Array[A]]): Decoder[Array[A]] = new SeqDecoder[A, Array](decodeA) {
    final protected def createBuilder(): Builder[A, Array[A]] = cbf.apply()
  }
  implicit final def decodeList[A](implicit decodeA: Decoder[A], cbf: CanBuildFrom[Nothing, A, List[A]]): Decoder[List[A]] = new SeqDecoder[A, List](decodeA) {
    final protected def createBuilder(): Builder[A, List[A]] = cbf.apply()
  }
  implicit final def decodeSeq[A](implicit decodeA: Decoder[A], cbf: CanBuildFrom[Nothing, A, Seq[A]]): Decoder[Seq[A]] = new SeqDecoder[A, Seq](decodeA) {
    final protected def createBuilder(): Builder[A, Seq[A]] = cbf.apply()
  }
  implicit final def decodeSet[A](implicit decodeA: Decoder[A], cbf: CanBuildFrom[Nothing, A, Set[A]]): Decoder[Set[A]] = new SeqDecoder[A, Set](decodeA) {
    final protected def createBuilder(): Builder[A, Set[A]] = cbf.apply()
  }
  implicit final def decodeVector[A](implicit decodeA: Decoder[A], cbf: CanBuildFrom[Nothing, A, Vector[A]]): Decoder[Vector[A]] = new SeqDecoder[A, Vector](decodeA) {
    final protected def createBuilder(): Builder[A, Vector[A]] = cbf.apply()
  }
  

  private[this] final val rightNone: Either[DecodingFailure, Option[Nothing]] = Right(None)

  implicit final def decodeOption[A](implicit d: Decoder[A]): Decoder[Option[A]] = new Decoder[Option[A]] {
    def apply(c: HCursor): Result[Option[A]] = c.value match {
      case StepNull => rightNone
      case _ => d(c) match {
        case Right(a) => Right(Some(a))
        case Left(df) => Left(df)
      }
    }
  }
  /*implicit final def decodeRefined[T, P, F[_, _]](
    implicit
    underlying: Decoder[T],
    v:          Validate[T, P],
    refType:    RefType[F]): Decoder[F[T, P]] = new Decoder[F[T, P]] {
    def apply(c: HCursor): Result[F[T, P]] = underlying(c) match {
      case Right(a) => refType.refine(a) match {
        case Right(a) => Right(a)
        case Left(e)  => Left(DecodingFailure("Refined " + e, c.history))
      }
      case Left(df) => Left(df)
    }
  }*/

  implicit final def decodeRefined[T, P](implicit underlying: Decoder[T]): Decoder[Refined[T, P]] = new Decoder[Refined[T, P]] {
    def apply(c: HCursor): Result[Refined[T, P]] = underlying(c) match {
      case Right(a) => Right(Refined.unsafeApply(a))
      case Left(df) => Left(df)
    }
  }

  implicit final def decodeHList[A, H <: HList](implicit gen: Generic.Aux[A, H], hEncoder: Decoder[H]): Decoder[A] =
    hEncoder.map(gen.from(_))

  implicit class RichDecoder[A](da: Decoder[A]) {
    def |[B >: A, C <: B](db: => Decoder[C]): Decoder[B] = instance[B] { c =>
      da(c) match {
        case Right(r) => Right(r)
        case Left(ta) => db(c) match {
          case z @ Right(r) => z
          case Left(tb)     => Left(DecodingFailure(s"${ta.message} | ${tb.message}", c.history))
        }
      }
    }
  }

  implicit final def decodeAdtNoDiscr[A, Repr <: Coproduct](implicit
    gen: Generic.Aux[A, Repr],
                                                            decodeRepr: Decoder[Repr]): Decoder[A] = decodeRepr.map(gen.from)
}

trait TupleDecoders {
  /*final def forProduct2[A, B, Target](f: (A, B) => Target)(implicit decoderA: Decoder[A], decoderB: Decoder[B]): Decoder[Target] = new Decoder[Target] {
    final def apply(a: Step): Decoder.Result[Target] = a match {
      case StepArray(array) if array.size == 2 =>
        Try((decoderA(array(0)), decoderB(array(1)))) match {
          case Success((Right(r0), Right(r1))) => Right(f(r0, r1))
          case Failure(e)                      => Left(DecodingFailure("forProduct2", a))
        }
      case _ => Left(DecodingFailure("forProduct2", a))
    }
  }*/
}
trait HListInstancesDecoder extends LowPriorityHListInstancesDecoder {
  //implicit final def decodeSingletonHList[H](implicit decodeH: Decoder[H]): Decoder[H :: HNil] =
  //decodeH.map(t => t::HNil).withErrorMessage("HList")
  //Decoder[Tuple1[H]].map(t => t._1 :: HNil).withErrorMessage("HList")
}
trait LowPriorityHListInstancesDecoder {
  implicit final val decodeHNil: Decoder[HNil] = Decoder.const(HNil)
  implicit final def decodeHCons[H, T <: HList](implicit decodeH: Decoder[H], decodeT: Decoder[T]): Decoder[H :: T] = new Decoder[H :: T] {
    def apply(c: HCursor): Decoder.Result[H :: T] = {
      val first = c.downArray
      Decoder.resultInstance.map2(first.as(decodeH), decodeT.tryDecode(first.delete))(_ :: _)
    }
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[H :: T] = {
      val first = c.downArray
      AccumulatingDecoder.resultInstance.map2(
        decodeH.tryDecodeAccumulating(first),
        decodeT.tryDecodeAccumulating(first.delete))(_ :: _)
    }
  }

  implicit final val decoderCNil: Decoder[CNil] = new Decoder[CNil] {
    def apply(c: HCursor): Decoder.Result[CNil] = Left(DecodingFailure("CNil", c.history))
  }

  implicit final def decodeCCons[L, R <: Coproduct](implicit
    decodeL: Decoder[L],
                                                    decodeR: Decoder[R]): Decoder[L :+: R] =
    decodeL.map(Inl(_)).or(decodeR.map(Inr(_)))
}