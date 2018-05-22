package com.iz2use.express.p21.generic.decoder

import cats.Contravariant
import shapeless.{ HList, Lazy, HNil, :: }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import shapeless.Generic
import com.iz2use.express.p21._
import com.iz2use.express.syntax._

abstract class ReprDecoder[A] extends Decoder[A]

final object ReprDecoder {
  def apply[A](implicit decoder: ReprDecoder[A]): ReprDecoder[A] = decoder

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