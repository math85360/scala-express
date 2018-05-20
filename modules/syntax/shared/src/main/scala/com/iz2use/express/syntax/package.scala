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

  object sizeOf {
    def apply[C](c: C): Int = 0
  }

  object hiIndex {
    def apply[C](c: C): Int = 0
  }

  object loIndex {
    def apply[C](c: C): Int = 0
  }

  sealed trait Source[A] {
    type Item
    type Repr
    def apply(predicate: Item => Boolean): Repr
  }
  object Source {
    //type Aux[A, B] = Source[A] { type Repr = B }
    /*private final def instance[A, B] = new Source[A] {
      type Repr = B
    }*/
    implicit final def optionRefinedList[A, P]: Source[Option[Refined[List[A], P]]] = new Source[Option[Refined[List[A], P]]] {
      type Item = A
      type Repr = List[A]
      def apply(predicate: Item => Boolean) :Repr = List[A]()
    }
    //implicit final val
    //implicit def apply[A](implicit src: Source[A]): Aux[A, src.Repr] = src
  }
  object query {
    //def apply[S, T](source: S)(predicate: T => Boolean)(implicit ev: Source.Aux[S, T]): T = null.asInstanceOf[T]
    def apply[S: Source](source: S): Source[S] = implicitly[Source[S]]
  }

  /*object nvl {
    def apply[C](a: Option[C], b: C): C = a.getOrElse(b)
  }*/

  /*implicit def withRefined[C <: ExpressType, T, P](in: T)(implicit
    f: Refined[T, P] => C,
                                                          v: Validate[T, P]): C =
                                                          f(Refined.unsafeApply(in))*/
}