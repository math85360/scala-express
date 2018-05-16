package com.iz2use.express

import eu.timepit.refined.api._

package object syntax {
  /*trait RefTo[+A]
  case class InsideRefTo[A](id: Int) extends RefTo[A]
  case class RefToDerived[A]() extends RefTo[A]*/
  case class RefTo[+A](id: Long)
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

  implicit def notFoundPredicate[A]: Validate.Plain[A, NotFound] =
    Validate.alwaysPassed(NotFound())

  case class Binary()
}