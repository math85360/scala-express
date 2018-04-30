package com.iz2use.express.step

import scala.util.Try
import scala.util.Success
import scala.util.Failure

trait Decoder[A] { self =>
  def apply(c: Step): Decoder.Result[A]

  /*def tryDecodeAccumulating(c: Step): AccumulatingDecoder.Result[A] = f(c) match {
      case Right(v) => Validated.valid(v)
      case Left(e) => Validated.invalidNel(e)
    }*/

  /*final def map[B](f: A=>B):Decoder[B] = new Decoder[B] {
    final
  }*/
}

final object Decoder extends TupleDecoders {

  type Result[A] = Either[DecodingFailure, A]

  implicit final val decodeBoolean: Decoder[Boolean] = new Decoder[Boolean] {
    final def apply(a: Step): Result[Boolean] = a match {
      case StepBoolean(boolean) => Right(boolean)
      case _                    => Left(DecodingFailure("Boolean", a))
    }
  }
  implicit final val decodeString: Decoder[String] = new Decoder[String] {
    final def apply(a: Step): Result[String] = a match {
      case StepString(string) => Right(string)
      case _                  => Left(DecodingFailure("String", a))
    }
  }
}

trait TupleDecoders {
  final def forProduct2[A, B, Target](f: (A, B) => Target)(implicit decoderA: Decoder[A], decoderB: Decoder[B]): Decoder[Target] = new Decoder[Target] {
    final def apply(a: Step): Decoder.Result[Target] = a match {
      case StepArray(array) if array.size == 2 =>
        Try((decoderA(array(0)), decoderB(array(1)))) match {
          case Success((Right(r0), Right(r1))) => Right(f(r0, r1))
          case Failure(e)                      => Left(DecodingFailure("forProduct2", a))
        }
      case _ => Left(DecodingFailure("forProduct2", a))
    }
  }
} 
