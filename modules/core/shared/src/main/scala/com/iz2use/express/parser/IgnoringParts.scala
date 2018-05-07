package com.iz2use.express.parser

import scala.language.postfixOps
import fastparse.WhitespaceApi

/*object IgnoringParts extends WhitespaceApi.Wrapper({
  import fastparse.all._
  import BasicAlphabetDefinition._

  spaceOrComments.rep(1)
    /*Types.embedded_remark.map(_ => ())).rep*/ .opaque("<any spaces or comments>") /*|
    (&(CharIn(";,:]")) | End)*/
}) {
  /*import fastparse.all._
  import fastparse.core.Implicits._
  import fastparse.core.ParserApiImpl
  import BasicAlphabetDefinition._

  implicit class RichParser[T](p0: Parser[T]) extends ParserApiImpl[T, Char, String](p0) {
    def ?~[V, R](p: P[V])(
      implicit
      evSeq: Sequencer[T, V, R],
      opt:   Optioner[Unit, Unit]): P[R] = {
      assert(p != null)
      p0 ~ spaceOrComments.?(opt) ~ p
    }
    
    def ?~/[V, R](p: P[V])(implicit ev: Sequencer[T, V,R], opt: Optioner[Unit, Unit]): P[R] ={
      assert(p !=null)
      (p0 ~ spaceOrComments.?(opt) ~ p ~/)
    }
  }*/
}*/