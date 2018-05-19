package com.iz2use.express.p21

import eu.timepit.refined._
import eu.timepit.refined.api._
import scala.language.higherKinds
import shapeless.{ Default, Strict, Generic, HList, LabelledGeneric, Lazy, HNil, ::, Coproduct, :+:, CNil, Inl, Inr }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import scala.collection.immutable.Set
import com.iz2use.express.syntax._

trait Encoder[A] { self =>
  def apply(a: A)(implicit strictness: EncoderStrictness): Step

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    final def apply(a: B)(implicit strictness: EncoderStrictness) = self(f(a))
  }

  final def mapStep(f: Step => Step): Encoder[A] = new Encoder[A] {
    final def apply(a: A)(implicit strictness: EncoderStrictness) = f(self(a))
  }
}

final object Encoder extends MidPriorityImplicitGenericEncoder {
  def apply[A](implicit encoder: Encoder[A]): Encoder[A] =
    encoder

  def instance[A](f: A => Step): Encoder[A] =
    new Encoder[A] {
      def apply(a: A)(implicit strictness: EncoderStrictness): Step =
        f(a)
    }

  def flatMap[A](f: A => Encoder[A]): Encoder[A] =
    new Encoder[A] {
      def apply(a: A)(implicit strictness: EncoderStrictness): Step =
        f(a)(a)
    }

  implicit final val encodeUnit: Encoder[Unit] = instance(_ => Step.fromEmpty)
  implicit final val encodeBoolean: Encoder[Boolean] = instance(Step.fromBoolean)

  implicit final val encodeString: Encoder[String] = instance(Step.fromString)

  implicit final val encodeInt: Encoder[Int] = instance(Step.fromInt)
  implicit final val encodeLong: Encoder[Long] = instance(Step.fromLong)
  implicit final val encodeFloat: Encoder[Float] = instance(Step.fromFloat)
  implicit final val encodeDouble: Encoder[Double] = instance(Step.fromDouble)
  implicit final val encodeLogical: Encoder[Logical] = instance[Logical] {
    case Logical.True    => StepBoolean(true)
    case Logical.False   => StepBoolean(false)
    case Logical.Unknown => StepUnknown

  }
  implicit final val encodeBinary: Encoder[Binary] = instance[Binary] {
    case _ => StepNull
  }
  final val encodeLiteral: Encoder[String] = instance(Step.fromLiteral)

  implicit final def encodeRefTo[A]: Encoder[RefTo[A]] = instance[RefTo[A]] { v =>
    Step.fromReference(v.id)
  }

  implicit final def encodeOption[A](implicit encodeA: Encoder[A]) = new Encoder[Option[A]] {
    def apply(a: Option[A])(implicit strictness: EncoderStrictness): Step = a match {
      case Some(v) => encodeA(v)
      case None    => StepNull
    }
  }

  implicit final def encodeSome[A](implicit encodeA: Encoder[A]) = new Encoder[Some[A]] {
    def apply(a: Some[A])(implicit strictness: EncoderStrictness): Step = a match {
      case Some(v) => encodeA(v)
    }
  }

  implicit final val encodeNone = instance[None.type] { case None => StepNull }

  final def deriveEncoder[A](implicit encoder: Lazy[generic.encoder.DerivedObjectEncoder[A]]): Encoder[A] = encoder.value

  /*implicit final def encodeTraversable[A, C[_]](implicit encodeA: Encoder[A]): Encoder[Traversable[A]] =
      instance[B[A]] { traversable =>
        traversable.
  }*/
  implicit final def encodeArray[A](implicit encodeA: Encoder[A]): ArrayEncoder[Array[A]] =
    new IterableArrayEncoder[A, Array](encodeA) {
      final protected def toIterator(a: Array[A]): Iterator[A] = a.toIterator
    }
  implicit final def encodeList[A](implicit encodeA: Encoder[A]): ArrayEncoder[List[A]] =
    new IterableArrayEncoder[A, List](encodeA) {
      final protected def toIterator(a: List[A]): Iterator[A] = a.toIterator
    }
  implicit final def encodeSeq[A](implicit encodeA: Encoder[A]): ArrayEncoder[Seq[A]] =
    new IterableArrayEncoder[A, Seq](encodeA) {
      final protected def toIterator(a: Seq[A]): Iterator[A] = a.toIterator
    }
  implicit final def encodeSet[A](implicit encodeA: Encoder[A]): ArrayEncoder[Set[A]] =
    new IterableArrayEncoder[A, Set](encodeA) {
      final protected def toIterator(a: Set[A]): Iterator[A] = a.toIterator
    }
  implicit final def encodeVector[A](implicit encodeA: Encoder[A]): ArrayEncoder[Vector[A]] =
    new IterableArrayEncoder[A, Vector](encodeA) {
      final protected def toIterator(a: Vector[A]): Iterator[A] = a.toIterator
    }
  protected[this] abstract class IterableArrayEncoder[A, C[_]](encodeA: Encoder[A]) extends ArrayEncoder[C[A]] {
    protected def toIterator(a: C[A]): Iterator[A]

    final def encodeArray(a: C[A])(implicit strictness: EncoderStrictness): Vector[Step] = {
      val builder = Vector.newBuilder[Step]
      val iterator = toIterator(a)
      while (iterator.hasNext) {
        builder += encodeA(iterator.next())
      }

      builder.result()
    }
  }
}
trait MidPriorityImplicitGenericEncoder extends LowPriorityImplicitGenericEncoder {
  implicit final def encodeRefined[E, T, P, F[_, _]](implicit
    underlying: Encoder[T],
                                                     v:           Validate[T, P],
                                                     refType:     RefType[F],
                                                     recoverable: Recoverable[E, T, P, F]): Encoder[F[T, P]] =
    new Encoder[F[T, P]] {
      def apply(a: F[T, P])(implicit strictness: EncoderStrictness): Step =
        strictness(refType.unwrap(a))(v, refType, recoverable) match {
          case Right(v)  => underlying(refType.unwrap(v))
          case Left(err) => throw new Error(s"Strictness on refined is too high for $err, or need recoverable")
        }
    }
}
trait LowPriorityImplicitGenericEncoder extends CoproductEncoder {

  implicit def genericObjectEncoder[A, H <: HList](
    implicit
    generic:  Generic.Aux[A, H],
    hEncoder: Lazy[ObjectEncoder[H]]): Encoder[A] =
    new Encoder[A] {
      def apply(value: A)(implicit strictness: EncoderStrictness): StepObject = {
        hEncoder.value.encodeObject(generic.to(value))
      }
    }

  /*implicit def coproductObjectEncoder[H, T <: Coproduct](implicit
    hEncoder: Lazy[Encoder[H]],
                                                         tEncoder: ObjectEncoder[T]): ObjectEncoder[H :+: T] = ObjectEncoder.instance {
    case Inl(h) => StepObject("", Vector(hEncoder.value(h)))
    case Inr(t) => tEncoder.encodeObject(t)
  }*/

}

/*trait LowestPriorityImplicitGenericEncoder {
  implicit def encodeAdtNoDiscr[A, Repr <: Coproduct](implicit
    gen: Generic.Aux[A, Repr],
                                                      encodeRepr: Encoder[Repr]): Encoder[A] =
    encodeRepr.contramap(gen.to)

}*/

trait CoproductEncoder {
  implicit final val encodeCNil: Encoder[CNil] =
    new Encoder[CNil] {
      def apply(a: CNil)(implicit strictness: EncoderStrictness): Step =
        throw new Exception("Inconceivable !")
    }

  implicit final def encodeCCons[L, R <: Coproduct](implicit
    encodeL: Encoder[L],
                                                    encodeR: Encoder[R]): Encoder[L :+: R] = new Encoder[L :+: R] {
    def apply(a: L :+: R)(implicit strictness: EncoderStrictness): Step = a match {
      case Inl(l) => encodeL(l)
      case Inr(r) => encodeR(r)
    }
  }
}