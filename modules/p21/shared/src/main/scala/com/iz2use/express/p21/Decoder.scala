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
  def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[A]

  private[p21] def decodeAccumulating(c: HCursor)(implicit strictness: DecoderStrictness): AccumulatingDecoder.Result[A] = apply(c) match {
    case Right(a) => Validated.valid(a)
    case Left(e)  => Validated.invalidNel(e)
  }

  def tryDecode(c: ACursor)(implicit strictness: DecoderStrictness): Decoder.Result[A] = c match {
    case hc: HCursor => apply(hc)
    case _ => Left(
      DecodingFailure("Attempt to decode value on failed cursor", c.history))
  }

  def tryDecodeAccumulating(c: ACursor)(implicit strictness: DecoderStrictness): AccumulatingDecoder.Result[A] = c match {
    case hc: HCursor => decodeAccumulating(hc)
    case _ => Validated.invalidNel(
      DecodingFailure("Attempt to decode value on failed cursor", c.history))
  }

  final def or[AA >: A](d: => Decoder[AA]): Decoder[AA] = new Decoder[AA] {
    final def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[AA] = self(c) match {
      case r @ Right(_) => r
      case Left(_)      => d(c)
    }
  }

  final def accumulating: AccumulatingDecoder[A] = AccumulatingDecoder.fromDecoder(self)

  def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    final def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[B] = tryDecode(c)
    override def tryDecode(c: ACursor)(implicit strictness: DecoderStrictness): Decoder.Result[B] = self.tryDecode(c) match {
      case Right(a)    => Right(f(a))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
    }
    override def decodeAccumulating(c: HCursor)(implicit strictness: DecoderStrictness): AccumulatingDecoder.Result[B] =
      tryDecodeAccumulating(c)

    override final def tryDecodeAccumulating(c: ACursor)(implicit strictness: DecoderStrictness): AccumulatingDecoder.Result[B] =
      self.tryDecodeAccumulating(c).map(f)
  }

  def collect[B](pf: PartialFunction[A, B]): Decoder[B] = new Decoder[B] {
    final def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[B] = tryDecode(c)
    override def tryDecode(c: ACursor)(implicit strictness: DecoderStrictness): Decoder.Result[B] = self.tryDecode(c) match {
      case Right(a) =>
        if (pf.isDefinedAt(a)) Right(pf(a))
        else Left(DecodingFailure("MatchError", c.history))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
    }
    override def decodeAccumulating(c: HCursor)(implicit strictness: DecoderStrictness): AccumulatingDecoder.Result[B] =
      tryDecodeAccumulating(c)

    override final def tryDecodeAccumulating(c: ACursor)(implicit strictness: DecoderStrictness): AccumulatingDecoder.Result[B] =
      self.tryDecodeAccumulating(c) match {
        case v @ Validated.Valid(a) =>
          Validated.condNel(
            pf.isDefinedAt(a),
            pf(a),
            DecodingFailure("Attempt to collect", c.history))
        case l @ Validated.Invalid(_) => l
      }
  }

  final def withErrorMessage(message: String): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[A] = self(c) match {
      case r @ Right(_) => r
      case Left(e)      => Left(DecodingFailure(message + " " + e.message, e.history))
    }

    override def decodeAccumulating(c: HCursor)(implicit strictness: DecoderStrictness): AccumulatingDecoder.Result[A] =
      self.decodeAccumulating(c).leftMap(_.map(e => DecodingFailure(message, e.history)))
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

final object Decoder extends MidPriorityDecoder {

  type Result[A] = Either[DecodingFailure, A]

  final val resultInstance: MonadError[Result, DecodingFailure] = catsStdInstancesForEither[DecodingFailure]

  def apply[A](implicit instance: Decoder[A]) = instance

  def instance[A](f: HCursor => Result[A]): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[A] = f(c)
  }
  def instancePf[A](name: String)(f: PartialFunction[Step, A]): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[A] = {
      f.isDefinedAt(c.value) match {
        case true  => Right(f(c.value))
        case false => Left(DecodingFailure(name, c.history))
      }
    }
  }
  /*def decodeObject[A](f: StepObject => A): Decoder[A] = instance { c =>

  }*/
  //def decodeHList[L >: HList]
  def alwaysFailed[A](name: String): Decoder[A] = instance { c =>
    Left(DecodingFailure(name, c.history))
  }

  def const[A](a: A): Decoder[A] = new Decoder[A] {
    def apply(c: HCursor)(implicit strictness: DecoderStrictness): Result[A] = Right(a)
    final override def decodeAccumulating(c: HCursor)(implicit strictness: DecoderStrictness): AccumulatingDecoder.Result[A] =
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
  
  implicit final val decodeBinary: Decoder[Binary] = instancePf("Binary") {
    case StepUnknown => Binary()
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

  final def deriveDecoder[A](implicit decoder: Lazy[DerivedDecoder[A]]): Decoder[A] = decoder.value

  implicit final def decodeCanBuildFrom[A, C[_]](implicit
    d: Decoder[A],
                                                 cbf: CanBuildFrom[Nothing, A, C[A]]): Decoder[C[A]] = new SeqDecoder[A, C](d, cbf)

  implicit final def decodeSet[A: Decoder]: Decoder[Set[A]] =
    decodeCanBuildFrom[A, List].map(_.toSet).withErrorMessage("[A]Set[A]")

  implicit final def decodeList[A: Decoder]: Decoder[List[A]] =
    decodeCanBuildFrom[A, List].withErrorMessage("[A]List[A]")
    
  implicit final def decodeVector[A: Decoder]: Decoder[Vector[A]] =
    decodeCanBuildFrom[A, Vector].withErrorMessage("[A]Vector[A]")

  /*final def deriveHList[A](implicit decoder: L

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
  }*/

  private[this] final val rightNone: Either[DecodingFailure, Option[Nothing]] = Right(None)

  implicit final def decodeOption[A](implicit d: Decoder[A]): Decoder[Option[A]] = new Decoder[Option[A]] {
    def apply(c: HCursor)(implicit strictness: DecoderStrictness): Result[Option[A]] = c.value match {
      case StepNull => rightNone
      case _ => d(c) match {
        case Right(a) => Right(Some(a))
        case Left(df) => Left(df)
      }
    }
  }

  implicit class RichDecoder[A](da: Decoder[A]) {
    def |[B >: A, C <: B](db: => Decoder[C]): Decoder[B] = new Decoder[B] {
      def apply(c: HCursor)(implicit strictness: DecoderStrictness): Result[B] = {
        da(c) match {
          case Right(r) => Right(r)
          case Left(ta) => db(c) match {
            case z @ Right(r) => z
            case Left(tb)     => Left(DecodingFailure(s"${ta.message} | ${tb.message}", c.history))
          }
        }
      }
    }
  }

}

trait MidPriorityDecoder extends LowPriorityDecoder {
  implicit final def decodeRefined[T, P, F[_, _]](
    implicit
    underlying:  Decoder[T],
    v:           Validate[T, P],
    refType:     RefType[F],
    recoverable: Recoverable[_, T, P, F]): Decoder[F[T, P]] = new Decoder[F[T, P]] {
    def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[F[T, P]] = underlying(c) match {
      case Right(a) =>
        strictness(a)(v, refType, recoverable) match {
          case Right(b) => Right(b)
          case Left(e)  => Left(DecodingFailure("Refined " + e, c.history))
        }
      case Left(df) => Left(df)
    }
  }

}
abstract class ReprDecoder[A] extends Decoder[A]
final object ReprDecoder {
  def apply[A](implicit decoder: ReprDecoder[A]):ReprDecoder[A] = decoder
  
  implicit final val decodeHNil: ReprDecoder[HNil] = new ReprDecoder[HNil] {
    def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[HNil] = Right(HNil)
  }

  implicit final def decodeHCons[H, T <: HList](implicit decodeH: Decoder[H], decodeT: Lazy[ReprDecoder[T]]): ReprDecoder[H :: T] =
    new ReprDecoder[H :: T] {
      def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[H :: T] = {
        val first = c.downArray
        Decoder.resultInstance.map2(first.as(decodeH, strictness), decodeT.value.tryDecode(first.delete))(_ :: _)
      }
      override def decodeAccumulating(c: HCursor)(implicit strictness: DecoderStrictness): AccumulatingDecoder.Result[H :: T] = {
        val first = c.downArray
        AccumulatingDecoder.resultInstance.map2(
          decodeH.tryDecodeAccumulating(first),
          decodeT.value.tryDecodeAccumulating(first.delete))(_ :: _)
      }
    }
}
abstract class DerivedDecoder[A] extends Decoder[A]
final object DerivedDecoder {
  implicit final def deriveDecoder[A, R](implicit
    gen: Generic.Aux[A, R],
                                         decode: Lazy[ReprDecoder[R]]): DerivedDecoder[A] = new DerivedDecoder[A] {
    def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[A] = decode.value(c) match {
      case Right(r) => Right(gen.from(r))
      case Left(e)  => Left(e)
    }
    override def decodeAccumulating(c: HCursor)(implicit strictness: DecoderStrictness): AccumulatingDecoder.Result[A] = {
      decode.value.decodeAccumulating(c).map(gen.from)
    }
  }
}
trait LowPriorityDecoder extends LowestPriorityDecoder {
  /*implicit final def decodeHList[A, H <: HList](implicit gen: Generic.Aux[A, H], hEncoder: Lazy[ReprDecoder[H]]): Decoder[A] =
    hEncoder.value.map(gen.from(_))*/

  implicit final val decoderCNil: Decoder[CNil] = new Decoder[CNil] {
    def apply(c: HCursor)(implicit strictness: DecoderStrictness): Decoder.Result[CNil] = Left(DecodingFailure("CNil", c.history))
  }

  implicit final def decodeCCons[L, R <: Coproduct](implicit
    decodeL: Decoder[L],
                                                    decodeR: Decoder[R]): Decoder[L :+: R] =
    decodeL.map(Inl(_)).or(decodeR.map(Inr(_)))
}

trait LowestPriorityDecoder {

}