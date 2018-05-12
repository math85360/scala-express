package com.iz2use.express.p21.parser

import scala.language.postfixOps
import fastparse.WhitespaceApi

object IgnoringParts extends WhitespaceApi.Wrapper({
  import fastparse.all._
  NoTrace((CharsWhileIn(" \t\n\f\r", 1) | "/*" ~/ (!"*/" ~ AnyChar).rep ~ "*/" ~/).rep(1))
})
