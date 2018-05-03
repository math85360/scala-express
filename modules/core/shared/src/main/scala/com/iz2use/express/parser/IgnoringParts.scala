package com.iz2use.express.parser

import scala.language.postfixOps
import fastparse.WhitespaceApi

object IgnoringParts extends WhitespaceApi.Wrapper({
  import fastparse.all._
  NoTrace((CharsWhileIn(" \t\n\f\r", 1) | "(*" ~/ (!"*)" ~ AnyChar).rep ~ "*)" ~/).rep(1))
})