package com.iz2use.express

//import fastparse.noApi._
//import parser.IgnoringParts._
import scala.language.postfixOps
import fastparse.all._
import fastparse.core.Implicits
import fastparse.core.Implicits.Optioner
import fastparse.core.Implicits.Sequencer

package object parser {

  /*case class Symbol(symbol: String) {
    val symbolParser = P(symbol)
  }*/
  object Symbol {
    def apply[R](p: P[R], spaceAfter: Boolean = true): P[R] = { //Symbol(p)
      import BasicAlphabetDefinition._
      if (!spaceAfter) spaceOrComments.?(Optioner.UnitOptioner) ~ p ~/
      else spaceOrComments.?(Optioner.UnitOptioner) ~ p ~
        spaceOrComments.?(Optioner.UnitOptioner) ~/
    }
  }
  object Keyword {
    def apply(p: P[Unit]): P[Unit] = {
      import BasicAlphabetDefinition._
      spaceOrComments ~ P(p) ~ spaceOrComments ~/
    }
  }
  //object discardResult extends  Optioner.Optioner[Unit, Unit]
  implicit final class RichParser[T](p0: Parser[T]) {
    def nonEmptyList: P[Seq[T]] = {
      //implicit val seq = Implicits.Sequencer.Sequencer1[T, Seq[T]]
      //implicit val rpt = Implicits.Repeater.GenericRepeater[T]
      //(p ~ ("," ~ p).rep).map({ case (head, tail) => head +: tail })
      import BasicAlphabetDefinition._
      p0.rep(1, spaceOrComments.? ~ "," ~ spaceOrComments.?)
    }
    def to[V](v: V)(implicit ev: T =:= Unit): P[V] = p0.map(_ => v)
    def ~|~[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R] = {
      import BasicAlphabetDefinition._
      p0.~(spaceOrComments).~(p)
    }
    def ~|?~[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R] = {
      import BasicAlphabetDefinition._
      p0.~(spaceOrComments.?(Optioner.UnitOptioner)).~(p)
    }
    def ~|~/[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R] = {
      import BasicAlphabetDefinition._
      p0.~(spaceOrComments).~(p).~/
    }
    def ~|?~/[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R] = {
      import BasicAlphabetDefinition._
      p0.~(spaceOrComments.?(Optioner.UnitOptioner)).~(p).~/
    }
    //def ~|~/[V, R](p: Parser[V]
    /*def ~(s: Symbol): P[T] = {
      import BasicAlphabetDefinition._
      p0 ~~ spaceOrComments.?(Optioner.UnitOptioner) ~~ s.symbolParser
    }
    def ~!~/(s: Symbol): P[T] = {
      import BasicAlphabetDefinition._
      import fastparse.parsers.Combinators.Cut
      Cut[T, Char, String](
        p0 ~~ spaceOrComments.?(Optioner.UnitOptioner) ~~ s.symbolParser)
    }*/
  }
  implicit final class RichString(p: String) {
    def to[V](v: V): P[V] = (p: Parser[Unit]).map(_ => v)
    //def sp = null
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
