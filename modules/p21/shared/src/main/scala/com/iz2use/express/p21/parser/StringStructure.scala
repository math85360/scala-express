package com.iz2use.express.p21.parser

import fastparse.all._

trait StringStructure extends TokenDefinition {
  private final val ControlDirective = P(Page | Alphabet | Extended2 |
    Extended4 | Arbitrary)

  private final val Page = P(ReverseSolidus ~ "S" ~ ReverseSolidus ~ LatinCodePoint)

  private final val Alphabet = P(ReverseSolidus ~ "P" ~ Upper ~ ReverseSolidus)

  private final val Extended2 = P(ReverseSolidus ~ "X2" ~ ReverseSolidus ~
    HexTwo.rep(1) ~ EndExtended)

  private final val Extended4 = P(ReverseSolidus ~ "X4" ~ ReverseSolidus ~
    HexFour.rep(1) ~ EndExtended)

  private final val EndExtended = P(ReverseSolidus ~ "X0" ~ ReverseSolidus)

  private final val Arbitrary = P(ReverseSolidus ~ "X" ~ ReverseSolidus ~ HexOne)

  private final val HexOne = P(Hex ~ Hex)

  private final val HexTwo = P(HexOne ~ HexOne)

  private final val HexFour = P(HexTwo ~ HexTwo)

  private final val StringPart = P(StringChar |
    HighCodePoint |
    Apostrophe ~ Apostrophe |
    ReverseSolidus ~ ReverseSolidus |
    ControlDirective)

  protected final val String = P("'" ~ StringPart.rep ~ "'")
}
