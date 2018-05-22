package com.iz2use.express.p21.generic.decoder

import cats.Contravariant
import shapeless.{ HList, Lazy, HNil, ::, CNil, :+:, Coproduct, Inl, Inr }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import shapeless.Generic
import com.iz2use.express.p21._
import com.iz2use.express.syntax._

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
