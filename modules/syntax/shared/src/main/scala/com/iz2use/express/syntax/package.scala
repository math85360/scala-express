package com.iz2use.express

import eu.timepit.refined.api._

package object syntax {
  /*trait RefTo[+A]
  case class InsideRefTo[A](id: Int) extends RefTo[A]
  case class RefToDerived[A]() extends RefTo[A]*/
  case class RefTo[+A](id: Long)
  object RefTo {
    implicit val ordering = Ordering.by((r: RefTo[_]) => r.id)
  }
  sealed trait Logical
  object Logical {
    case object True extends Logical
    case object False extends Logical
    case object Unknown extends Logical
  }
  val True = Logical.True
  val False = Logical.False
  val Unknown = Logical.Unknown

  final case class NotFound()
  object NotFound {
    implicit def notFoundPredicate[A]: Validate.Plain[A, NotFound] =
      Validate.alwaysPassed(NotFound())
  }

  case class Binary()

  trait ExpressRoot
  trait ExpressEntity extends ExpressRoot with Product with Serializable
  trait ExpressEnumeration extends ExpressType
  trait ExpressFunction extends ExpressRoot
  trait ExpressRule extends ExpressRoot
  trait ExpressType extends ExpressRoot

  object sizeOf {
    def apply[C](c: C): Int = 0
  }

  object hiIndex {
    def apply[C](c: C): Int = 0
  }

  object loIndex {
    def apply[C](c: C): Int = 0
  }

  /*object nvl {
    def apply[C](a: Option[C], b: C): C = a.getOrElse(b)
  }*/

  /*implicit def withRefined[C <: ExpressType, T, P](in: T)(implicit
    f: Refined[T, P] => C,
                                                          v: Validate[T, P]): C =
                                                          f(Refined.unsafeApply(in))*/
}