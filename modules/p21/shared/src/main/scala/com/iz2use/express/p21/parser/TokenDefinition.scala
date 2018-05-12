package com.iz2use.express.p21.parser

import fastparse.all._

trait TokenDefinition extends RfcCommons {
private final val UserDefinedKeyword = P("!" ~ Name)

private final val StandardKeyword = P(Name)

private[parser] final val Keyword = P(UserDefinedKeyword | StandardKeyword)

private final val Sign = P(CharIn("+-"))

private[parser] final val Integer = P(Sign.? ~ Digits1)

private[parser] final val Real = P(Sign.? ~ Digits1 ~ "." ~ Digits0 ~
    ("E" ~ Sign.? ~ Digits1).?)

private[parser] final val EntityInstanceName = P("#" ~ Digits1)

private final val ValueInstanceName = P("@" ~ Digits1)

private final val ConstantEntityName = P("#" ~ InstanceName)

private final val ConstantValueName = P("#" ~ InstanceName)

private[parser] final val LhsOccurrenceName = P(EntityInstanceName | ValueInstanceName)

private[parser] final val RhsOccurrenceName = P(EntityInstanceName | ValueInstanceName |
    ConstantEntityName | ConstantValueName)

private[parser] final val AnchorName = P("<" ~
    UriFragmentIdentifier /*.filter(_.exists(!_.isDigit))*/ ~ ">")

private[parser] final val TagName = P(UpperOrLower ~ UpperOrLowerOrDigits.?)

private[parser] final val Resource = P("<" ~ UniversalResourceIdentifier ~ ">")

private[parser] final val Enumeration = P("." ~ Upper ~ UpperOrDigits.? ~ ".")

private final val HexChar = DigitChar ++ ('A' to 'F')
private[parser] final val Hex = P(CharIn(HexChar))

private[parser] final val Binary = P(CharIn('0' to '3') ~ CharsWhileIn(HexChar))

private[parser] final val SignatureContent = P(Base64)
  //private final val KEYWORD = P(USER
}
