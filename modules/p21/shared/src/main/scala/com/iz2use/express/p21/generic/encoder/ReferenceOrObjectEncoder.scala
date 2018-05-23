package com.iz2use.express.p21.generic.encoder

import cats.Contravariant
import shapeless.{ HList, Lazy, HNil, :: }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import shapeless.Generic
import com.iz2use.express.p21._
import com.iz2use.express.syntax._

abstract class ReferenceOrObjectEncoder[A, Repr <: HList] extends RootEncoder[A]

object ReferenceOrObjectEncoder {
  implicit def apply[A, Repr <: HList](name: String)(f: A => Repr)(implicit reprEncoder: ReprObjectEncoder[Repr]) = new ReferenceOrObjectEncoder[A, Repr] {
    final def apply(a: A)(implicit context: EncodingContext): Step = context.topLevel match {
      case true  => reprEncoder.encodeObject(f(a))(context.downLevel).copy(tpe = name)
      case false => context.referencer(a)
    }
  }
}
