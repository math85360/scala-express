package com.iz2use.express.p21

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.Left
import scala.Right

trait Decoder[A] { self =>
  def apply(c: Step): Decoder.Result[A]

  /*def orElse[B<:A, C>:B](other: Decoder[C]): Decoder[B] = apply(c) match {
    case Left(e) => other(c)
  }*/

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

  def instance[A](f: Step => Result[A]): Decoder[A] = new Decoder[A] {
    def apply(c: Step): Decoder.Result[A] = f(c)
  }
  def instancePf[A](name: String)(f: PartialFunction[Step, A]): Decoder[A] = instance { c =>
    f.isDefinedAt(c) match {
      case true  => Right(f(c))
      case false => Left(DecodingFailure(name, c))
    }
  }
  def alwaysFailed[A](name: String): Decoder[A] = instance { c =>
    Left(DecodingFailure(name, c))
  }

  implicit final val decodeBoolean: Decoder[Boolean] = instancePf("Boolean") {
    case StepBoolean(boolean) => boolean
  }
  implicit final val decodeString: Decoder[String] = instancePf("String") {
    case StepString(string) => string
  }
  implicit class RichDecoder[A](da: Decoder[A]) {
    def |[B >: A, C <: B](db: => Decoder[C]): Decoder[B] = instance[B] { c =>
      da(c) match {
        case Right(r) => Right(r)
        case Left(ta) => db(c) match {
          case z @ Right(r) => z
          case Left(tb)     => Left(DecodingFailure(s"${ta.message} | ${tb.message}", c))
        }
      }
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
