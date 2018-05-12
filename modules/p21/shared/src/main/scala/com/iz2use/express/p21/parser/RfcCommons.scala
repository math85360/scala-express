package com.iz2use.express.p21.parser

import scala.language.postfixOps
import fastparse.all._

trait RfcCommons extends EnhancedBasicAlphabetDefinition {
  private final val UriUnreserved = P(CharIn(UpperOrLowerChar ++ DigitChar ++ "-._~"))
  private final val UriPctEncoded = P("%" ~ CharIn(DigitChar ++ ('a' to 'f') ++ ('A' to 'F')).rep(2, max = 2))
  private final val UriSubDelims = P(CharIn("!$&'()*+,;="))
  private final val UriPchar = P(UriUnreserved | UriPctEncoded | UriSubDelims | ":" | "@")
  private[parser] final val UriFragmentIdentifier = P("#" ~ (UriPchar | "/" | "?").rep(1))
  private final val UriUserinfo = P((UriUnreserved | UriPctEncoded | UriSubDelims | ":").rep(1))
  private final val UriIPLiteral = P(Fail)
  private final val UriIPv4Address = P(Fail)
  private final val UriRegName = P((UriUnreserved | UriPctEncoded | UriSubDelims).rep(1))
  private final val UriHost = P(UriIPLiteral | UriIPv4Address | UriRegName)
  private final val UriPort = P(Fail)
  private final val UriAuthority = P((UriUserinfo ~ "@").? ~ UriHost ~ (":" ~ UriPort).?)
  private final val UriSegmentNz = P(UriPchar.rep(1))
  private final val UriSegment = P(UriSegmentNz.?)
  private final val UriSegmentNzs = P(("/" ~ UriSegment).rep)
  private final val UriPathAbempty = P(UriSegmentNzs)
  private final val UriPathAbsolute = P("/" ~ (UriSegmentNz ~ UriSegmentNzs))
  private final val UriPathRootLess = P(UriSegmentNz ~ UriSegmentNzs)
  private final val UriPathEmpty = P(Pass)
  private final val UriHierPart = P("//" ~ UriAuthority ~ UriPathAbempty |
    UriPathAbsolute |
    UriPathRootLess |
    UriPathEmpty)
  private final val UriQuery = P("?" ~ Fail)
  private final val UriScheme = P(UpperOrLower ~ (UpperOrLowerOrDigits | "+" | "-" | ".").rep)
  private[parser]final val UniversalResourceIdentifier = P(UriScheme ~ ":" ~ UriHierPart ~ UriQuery.? ~ UriFragmentIdentifier.?)
  private final val Base64Char = UpperChar ++ LowerChar ++ DigitChar ++ "+/="
  private[parser] final val Base64 = P(CharsWhileIn(Base64Char, 1))
}
