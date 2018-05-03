package com.iz2use.express

import fastparse.all._
import fastparse.core.Implicits.Optioner
import fastparse.core.Implicits.Sequencer

trait LowerPriorityImplicit {
  //implicit final def silentlyIgnoreParser[R <: ast.Node](p: Parser[_]): P[R] = p.asInstanceOf[P[R]]
}
package object parser extends LowerPriorityImplicit {
  implicit final class RichParser[T](p: Parser[T]) {
    def nonEmptyList[R](implicit ev: fastparse.core.Implicits.Repeater[T, R]): P[R] = p.rep(1, ",")
    def to[V](v: V)(implicit ev: T =:= Unit): P[V] = p.map(_ => v)
  }
  /*implicit final def ReduceMultipleOperationParser[O <: ast.Operator](p: P[(ast.Expression, Seq[(O, ast.Expression)])]): P[ast.Expression] =
    p.map {
      case (a, Seq()) => a
      case (a, b)     => ast.MultipleOperation(a, b)
    }*/

  /*implicit final def ReduceSingleOperationParser[O <: ast.Operator](p: P[(ast.Expression, Option[(O, ast.Expression)])]): P[ast.Expression] =
    p.map {
      case (a, Some((o, b))) => ast.SingleOperation(a, o, b)
      case (a, _)            => a
    }*/

  implicit final class RichString(p: String) {
    def to[V](v: V): P[V] = (p: Parser[Unit]).map(_ => v)
  }
  implicit final def toSeqOptionerImplicit[T] = new SeqOptionerImplicit[T]
  final class SeqOptionerImplicit[T] extends Optioner[Seq[T], Seq[T]] {
    def none = Nil
    def some(value: Seq[T]) = value
  }
  implicit final object BooleanOptionerImplicit extends Optioner[Unit, Boolean] {
    def none = false
    def some(value: Unit) = true
  }
  implicit def toUnitParser(p: Parser[_]): P[Unit] = p.map(_ => ())
  implicit def multipleOperationReducableSequencer[O <: ast.Operator] = new MultipleOperationReducableSequencer[O]
  class MultipleOperationReducableSequencer[O <: ast.Operator] extends Sequencer[ast.Expression, Seq[(O, ast.Expression)], ast.Expression] {
    override def apply(a: ast.Expression, b: Seq[(O, ast.Expression)]): ast.Expression = b match {
      case Nil => a
      case _   => ast.MultipleOperation(a, b)
    }
  }
  implicit def singleOperationReducableSequencer[O <: ast.Operator, B >: ast.Expression] = new SingleOperationReducableSequencer[O, B]
  class SingleOperationReducableSequencer[O <: ast.Operator, B >: ast.Expression] extends Sequencer[ast.Expression, Option[(O, B)], ast.Expression] {
    override def apply(a: ast.Expression, b: Option[(O, ast.Expression)]): ast.Expression = b match {
      case None         => a
      case Some((op, b)) => ast.SingleOperation(a, op, b)
    }
  }
  implicit def accSequencer[R, T <: R, V <: R] = new SeqSequencer[R, T, V]
  class SeqSequencer[R, T <: R, V <: R] extends Sequencer[Seq[T], Seq[V], Seq[R]] {
    def apply(a: Seq[T], b: Seq[V]): Seq[R] = a ++ b
  }

  //sealed trait Or[+A, +B]
  //type :+:[+A, +B] = Either[A, B]

}
