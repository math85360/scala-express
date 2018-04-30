package com.iz2use.express.step

import eu.timepit.refined._
import eu.timepit.refined.api.{ Refined, Validate }
import scala.language.higherKinds
import shapeless.{ Default, Strict, Generic, HList, LabelledGeneric, Lazy, HNil, ::, Coproduct, :+:, CNil, Inl, Inr }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import scala.collection.immutable.Set

trait Encoder[A] { self =>
  def apply(a: A): Step

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    final def apply(a: B) = self(f(a))
  }

  final def mapStep(f: Step => Step): Encoder[A] = new Encoder[A] {
    final def apply(a: A) = f(self(a))
  }
}

final object Encoder extends LowPriorityImplicitGenericEncoder {
  implicit final val encodeBoolean: Encoder[Boolean] = instance(Step.fromBoolean)

  implicit final val encodeString: Encoder[String] = instance(Step.fromString)

  implicit final val encodeInt: Encoder[Int] = instance(Step.fromInt)
  implicit final val encodeLong: Encoder[Long] = instance(Step.fromLong)
  implicit final val encodeFloat: Encoder[Float] = instance(Step.fromFloat)
  implicit final val encodeDouble: Encoder[Double] = instance(Step.fromDouble)

  implicit final def encodeOption[A](implicit encodeA: Encoder[A]) = instance[Option[A]] {
    case Some(v) => encodeA(v)
    case None    => StepNull
  }

  implicit final def encodeRefined[A, R](implicit encodeA: Encoder[A], validate: Validate[A, R]): Encoder[A Refined R] = {
    encodeA.contramap {
      case Refined(v) =>
        if (validate.isValid(v))
          v
        else
          throw new RuntimeException(s"Not validated : {$v} with ${validate.showExpr(v)}")
    }
  }

  /*implicit final def encodeTraversable[A, C[_]](implicit encodeA: Encoder[A]): Encoder[Traversable[A]] =
      instance[B[A]] { traversable =>
        traversable.
  }*/
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

    final def encodeArray(a: C[A]): Vector[Step] = {
      val builder = Vector.newBuilder[Step]
      val iterator = toIterator(a)
      while (iterator.hasNext) {
        builder += encodeA(iterator.next())
      }

      builder.result()
    }
  }
}

trait LowPriorityImplicitGenericEncoder {
  def apply[A](implicit encoder: Encoder[A]): Encoder[A] =
    encoder

  def instance[A](f: A => Step): Encoder[A] =
    new Encoder[A] {
      def apply(a: A): Step =
        f(a)
    }

  implicit def genericObjectEncoder[A, H <: HList](
    implicit
    generic:  Generic.Aux[A, H],
    hEncoder: Lazy[ObjectEncoder[H]]
  ): Encoder[A] =
    instance { value =>
      hEncoder.value.encodeObject(generic.to(value))
    }

  implicit val cnilObjectEncoder: ObjectEncoder[CNil] =
    ObjectEncoder.instance(cnil => throw new Exception("Inconceivable !"))

  implicit def coproductObjectEncoder[H, T <: Coproduct](implicit
    hEncoder: Lazy[Encoder[H]],
                                                         tEncoder: ObjectEncoder[T]): ObjectEncoder[H :+: T] = ObjectEncoder.instance {
    case Inl(h) => StepObject("", Vector(hEncoder.value(h)))
    case Inr(t) => tEncoder.encodeObject(t)
  }
} 
