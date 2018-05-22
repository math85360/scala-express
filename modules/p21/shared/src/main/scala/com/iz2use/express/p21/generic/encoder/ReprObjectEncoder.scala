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
    final def encodeObject(c: HNil)(implicit strictness: EncoderStrictness): StepObject = {
      StepObject("", Vector.empty)
    }
  }

  implicit def hlistObjectEncoder[H, T <: HList](
      implicit
      hEncoder: Encoder[H],
      tEncoder: Lazy[ReprObjectEncoder[T]]): ReprObjectEncoder[H :: T] = new ReprObjectEncoder[H :: T] { hlist =>
    final def encodeObject(hlist: H :: T)(implicit strictness: EncoderStrictness): StepObject = {
      val head = hEncoder.apply(hlist.head)
      val tail = tEncoder.value.encodeObject(hlist.tail)
      tail.copy(fields = head +: tail.fields)
    }
  }
  //implicit def deriveReprObjectEncoder[R]: ReprObjectEncoder[R] =
}
