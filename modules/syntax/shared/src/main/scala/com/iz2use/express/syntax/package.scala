package com.iz2use.express

import eu.timepit.refined.api._

package object syntax {
  /*trait RefTo[+A]
  case class InsideRefTo[A](id: Int) extends RefTo[A]
  case class RefToDerived[A]() extends RefTo[A]*/
  case class RefTo[+A](id: Long)
  object RefTo {
    implicit val ordering = Ordering.by((r: RefTo[_]) => r.id)
    implicit def materialize[A](in: RefTo[A]) /*(implicit db:DatabaseIfc)*/ : A = null.asInstanceOf[A]
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
  Nil

  def fromOptionRefinedTraversable[T, C[_], P](in: Option[Refined[C[T], P]], default: => C[T]): C[T] = in match {
    case Some(Refined(v)) => v
    case None             => default
  }
  implicit def fromOptionRefinedList[T, P](in: Option[Refined[List[T], P]]): List[T] =
    fromOptionRefinedTraversable[T, List, P](in, Nil)
  implicit def fromRefinedArray[T, P](in: Refined[Array[T], P]): Array[T] =
    in.value
  implicit def fromRefinedList[T, P](in: Refined[List[T], P]): List[T] =
    in.value
  implicit def fromRefinedSeq[T, P](in: Refined[Seq[T], P]): Seq[T] =
    in.value
  implicit def fromRefinedSet[T, P](in: Refined[Set[T], P]): Set[T] =
    in.value
  implicit def fromOptionBoolean(in: Option[Boolean]): Boolean = in.getOrElse(false)
  implicit def fromOptionEntity[T <: ExpressEntity](in: Option[T]): T = in.get
  //implicit def fromOption[T](in: Option[T]): T

  /*object nvl {
    def apply[C](a: Option[C], b: C): C = a.getOrElse(b)
  }*/

  /*implicit def withRefined[C <: ExpressType, T, P](in: T)(implicit
    f: Refined[T, P] => C,
                                                          v: Validate[T, P]): C =
                                                          f(Refined.unsafeApply(in))*/
}