package com.iz2use.express

import fastparse.noApi._
import parser.IgnoringParts._
import fastparse.core.Implicits
import fastparse.core.Implicits.Optioner
import fastparse.core.Implicits.Sequencer

package object parser {
  implicit final class RichParser[T](p: Parser[T]) {
    def nonEmptyList : P[Seq[T]] = {
      //implicit val seq = Implicits.Sequencer.Sequencer1[T, Seq[T]]
      //implicit val rpt = Implicits.Repeater.GenericRepeater[T]
      //(p ~ ("," ~ p).rep).map({ case (head, tail) => head +: tail })
      p.rep(1, ",")
    }
    def to[V](v: V)(implicit ev: T =:= Unit): P[V] = p.map(_ => v)
  }
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
  implicit def multipleOperationReducableSequencer[O <: ast.Operator] = new MultipleOperationReducableSequencer[O]
  class MultipleOperationReducableSequencer[O <: ast.Operator] extends Sequencer[ast.Expression, Seq[(O, ast.Expression)], ast.Expression] {
    override def apply(a: ast.Expression, b: Seq[(O, ast.Expression)]): ast.Expression = b match {
      case Nil => a
      case _   => ast.MultipleOperation(a, b)
    }
  }
  implicit def singleOperationReducableSequencer[O <: ast.Operator] = new SingleOperationReducableSequencer[O]
  class SingleOperationReducableSequencer[O <: ast.Operator] extends Sequencer[ast.Expression, Option[(O, ast.Expression)], ast.Expression] {
    override def apply(a: ast.Expression, b: Option[(O, ast.Expression)]): ast.Expression = b match {
      case None          => a
      case Some((op, b)) => ast.SingleOperation(a, op, b)
    }
  }
  /*implicit def accOneOrMore[R] = new AccOneOrMoreSequencer[R]
  class AccOneOrMoreSequencer[T] extends Sequencer[T, Seq[T], Seq[T]] {
    def apply(a: T, b: Seq[T]): Seq[T] = a +: b
  }*/
  /*implicit def accSequencer[R, T <: R, V <: R] = new SeqSequencer[R, T, V]
  class SeqSequencer[R, T <: R, V <: R] extends Sequencer[Seq[T], Seq[V], Seq[R]] {
    def apply(a: Seq[T], b: Seq[V]): Seq[R] = a ++ b
  }*/

  //sealed trait Or[+A, +B]
  //type :+:[+A, +B] = Either[A, B]
  object Parser extends ExpressFileStructure {

  }

}
