package com.iz2use.express.p21

import eu.timepit.refined.api._

trait Strictness {
  def apply[E, T, P, F[_, _]](in: T)(implicit
      v: Validate[T, P],
                                     refType:     RefType[F],
                                     recoverable: Recoverable[E, T, P, F]): Strictness.Result[F[T, P]]
}

sealed trait EncoderStrictness extends Strictness

sealed trait DecoderStrictness extends Strictness

object Strictness {
  type Result[A] = Either[String, A]

  sealed trait Flexible extends Strictness {
    override def apply[E, T, P, F[_, _]](in: T)(implicit
        v: Validate[T, P],
                                                refType:     RefType[F],
                                                recoverable: Recoverable[E, T, P, F]): Strictness.Result[F[T, P]] =
      Right(refType.unsafeWrap(in))
  }

  sealed trait Strict extends Strictness {
    override def apply[E, T, P, F[_, _]](in: T)(implicit
        v: Validate[T, P],
                                                refType:     RefType[F],
                                                recoverable: Recoverable[E, T, P, F]): Strictness.Result[F[T, P]] =
      refType.refine[P].apply(in)
  }

  sealed trait StrictOrConvert extends Strictness {
    override def apply[E, T, P, F[_, _]](in: T)(implicit
        v: Validate[T, P],
                                                refType:     RefType[F],
                                                recoverable: Recoverable[E, T, P, F]): Strictness.Result[F[T, P]] =
      refType.refine[P].apply(in) match {
        case r @ Right(_) => r
        case Left(e) => recoverable.recover(in) match {
          case r @ Right(_) => r
          case l @ Left(_)  => l
        }
      }
  }

  final val encodeFlexible = new EncoderStrictness with Flexible
  final val encodeStrictOrConvert = new EncoderStrictness with StrictOrConvert
  final val encodeStrict = new EncoderStrictness with Strict
  final val decodeFlexible = new DecoderStrictness with Flexible
  final val decodeStrictOrConvert = new DecoderStrictness with StrictOrConvert
  final val decodeStrict = new DecoderStrictness with Strict
}

/**
  * Where E is Entity and T the Type, because we don't want a bad
  */
trait Recoverable[E, T, P, F[_, _]] {
  def recover(in: T): Strictness.Result[F[T, P]]
}

trait LowestPriorityRecoverableImplicits {
  implicit def neverRecoverable[E, T, P, F[_, _]] = new Recoverable[E, T, P, F] {
    def recover(in: T): Strictness.Result[F[T, P]] = Left("Never recoverable")
  }
}

object Recoverable extends LowestPriorityRecoverableImplicits {

}
