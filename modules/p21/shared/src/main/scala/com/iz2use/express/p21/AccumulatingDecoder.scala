package com.iz2use.express.p21

import cats.{ ApplicativeError, Semigroup, SemigroupK }
import cats.data.{ NonEmptyList, Validated, ValidatedNel }

sealed trait AccumulatingDecoder[A] extends Serializable { self =>
  def apply(c: HCursor): AccumulatingDecoder.Result[A]
}

final object AccumulatingDecoder {
  final type Result[A] = ValidatedNel[DecodingFailure, A]

  implicit final def fromDecoder[A](implicit decode: Decoder[A]): AccumulatingDecoder[A] = new AccumulatingDecoder[A] {
    final def apply(c: HCursor): Result[A] = decode.decodeAccumulating(c)
  }

  final val failureNelInstance: Semigroup[NonEmptyList[DecodingFailure]] =
    NonEmptyList.catsDataSemigroupForNonEmptyList[DecodingFailure]

  final val resultInstance: ApplicativeError[Result, NonEmptyList[DecodingFailure]] =
    Validated.catsDataApplicativeErrorForValidated[NonEmptyList[DecodingFailure]](failureNelInstance)
}