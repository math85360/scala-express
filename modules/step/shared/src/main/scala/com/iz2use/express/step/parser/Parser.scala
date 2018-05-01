package com.iz2use.express.step.parser

import fastparse.WhitespaceApi
import fastparse.all.NoTrace

object IgnoringParts extends WhitespaceApi.Wrapper({
  import fastparse.all._
  NoTrace((CharsWhileIn(" \t\n\f", 1) | "/*" ~ (!"*/" ~ AnyChar).rep ~ "*/").rep(1))
})
