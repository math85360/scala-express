package com.iz2use.express.p21.generic.encoder

import cats.Contravariant
import shapeless.{ HList, Lazy, HNil, :: }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import shapeless.Generic
import com.iz2use.express.p21._
import com.iz2use.express.syntax._


abstract class DerivedObjectEncoder[A] extends ObjectEncoder[A]

final object DerivedObjectEncoder {
  final def toHList[A, R <: HList](name: String)(f: A => R)(implicit encode: Lazy[ObjectEncoder[R]]): DerivedObjectEncoder[A] = new DerivedObjectEncoder[A] {
    final def encodeObject(a: A)(implicit strictness: EncoderStrictness): StepObject = encode.value.encodeObject(f(a))
  }

  implicit def deriveEncoder[A, R](implicit
    gen: Generic.Aux[A, R],
                                   encode: Lazy[ReprObjectEncoder[R]]): DerivedObjectEncoder[A] = new DerivedObjectEncoder[A] {
    final def encodeObject(a: A)(implicit strictness: EncoderStrictness): StepObject = encode.value.encodeObject(gen.to(a))
  }
}
