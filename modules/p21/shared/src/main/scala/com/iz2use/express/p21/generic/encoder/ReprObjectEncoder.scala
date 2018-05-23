package com.iz2use.express.p21.generic.encoder

import cats.Contravariant
import shapeless.{ HList, Lazy, HNil, :: }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import shapeless.Generic
import com.iz2use.express.p21._
import com.iz2use.express.syntax._

abstract class ReprObjectEncoder[A] extends ObjectEncoder[A]

final object ReprObjectEncoder {
  def apply[A](implicit encoder: ReprObjectEncoder[A]): ReprObjectEncoder[A] = encoder

  implicit val hnilEncoder: ReprObjectEncoder[HNil] = new ReprObjectEncoder[HNil] {
    final def encodeObject(c: HNil)(implicit context: EncodingContext): StepObject = {
      StepObject("", Vector.empty)
    }
  }

  implicit def hlistObjectEncoder[H, T <: HList](
      implicit
      hEncoder: Encoder[H],
      tEncoder: Lazy[ReprObjectEncoder[T]]): ReprObjectEncoder[H :: T] = new ReprObjectEncoder[H :: T] { hlist =>
    final def encodeObject(hlist: H :: T)(implicit context: EncodingContext): StepObject = {
      val head = hEncoder.apply(hlist.head)
      val tail = tEncoder.value.encodeObject(hlist.tail)
      tail.copy(fields = head +: tail.fields)
    }
  }
}

abstract class ReferenceOrObjectEncoder[A, Repr <: HList] extends RootEncoder[A]

object ReferenceOrObjectEncoder {
  implicit def apply[A, Repr <: HList](name: String)(f: A => Repr)(implicit reprEncoder: ReprObjectEncoder[Repr]) = new ReferenceOrObjectEncoder[A, Repr] {
    final def apply(a: A)(implicit context: EncodingContext): Step = context.topLevel match {
      case true  => reprEncoder.encodeObject(f(a))(context.downLevel).copy(tpe = name)
      case false => Step.fromReference(-1)
    }
  }
}
/*
abstract class ReferenceEncoderOrNot[A] extends Encoder[A]
object ReferenceEncoderOrNot extends ReferenceEncoderOrNotMidPriorityImplicits {
  def forward[A](implicit encoder: Encoder[A]) : ReferenceEncoderOrNot[A] = new ReferenceEncoderOrNot[A] {
    final def apply(a: A)(implicit context: EncodingContext): Step = encoder(a)
  }
  implicit def fromObject[A](implicit reprObjectEncoder: Encoder[A], ev: A <:< ExpressEntity): ReferenceEncoderOrNot[A] = new ReferenceEncoderOrNot[A] {
    final def apply(a: A)(implicit context: EncodingContext): Step = {
      println(s"$a as reference")
      Step.fromReference(-1)
    }
  }
  implicit def fromList[A](implicit reprObjectEncoder: Encoder[List[A]], ev: A <:< ExpressEntity): ReferenceEncoderOrNot[List[A]] = forward
  implicit def fromOption[A](implicit reprObjectEncoder: Encoder[Option[A]], ev: A <:< ExpressEntity): ReferenceEncoderOrNot[Option[A]] = forward
  implicit def fromSeq[A](implicit reprObjectEncoder: Encoder[Seq[A]], ev: A <:< ExpressEntity): ReferenceEncoderOrNot[Seq[A]] = forward
  implicit def fromSet[A](implicit reprObjectEncoder: Encoder[Set[A]], ev: A <:< ExpressEntity): ReferenceEncoderOrNot[Set[A]] = forward

}

trait ReferenceEncoderOrNotMidPriorityImplicits extends ReferenceEncoderOrNotLowPriorityImplicits {

}

trait ReferenceEncoderOrNotLowPriorityImplicits {
  implicit def fromAny[A](implicit reprObjectEncoder: Encoder[A]): ReferenceEncoderOrNot[A] = new ReferenceEncoderOrNot[A] {
    final def apply(a: A)(implicit context: EncodingContext): Step = {
      println(s"$a as any")
      reprObjectEncoder(a)
    }
  }
}
*/
