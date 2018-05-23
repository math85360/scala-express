package com.iz2use.express.p21

import eu.timepit.refined._
import eu.timepit.refined.api._
import scala.language.higherKinds
import shapeless.{ Default, Strict, Generic, HList, LabelledGeneric, Lazy, HNil, ::, Coproduct, :+:, CNil, Inl, Inr }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import scala.collection.immutable.Set
import com.iz2use.express.syntax._

case class EncodingContext(strictness: EncoderStrictness, topLevel: Boolean, referencer: Any => StepReference) {
  def downLevel: EncodingContext = if (topLevel) this.copy(topLevel = false) else this
}
object EncodingContext {
  implicit def default(implicit strictness: EncoderStrictness): EncodingContext =
    EncodingContext(strictness, true, (a: Any) => StepReference(-1))
}
