package com.iz2use.express.step.parser

import scala.language.postfixOps
import fastparse.all._

trait BasicAlphabetDefinition {
  private[parser] final val SpaceChar = " "
  private final val Space = P(" ")

  private[parser] final val DigitChar = '0' to '9'
  private final val Digit = P(CharIn(DigitChar))

  private[parser] final val LowerChar = 'a' to 'z'
  private[parser] final val Lower = P(CharIn(LowerChar))

  private[parser] final val UpperChar = 'A' to 'Z'
  private[parser] final val Upper = P(CharIn(UpperChar))

  private[parser] final val SpecialChar = """!"*$%&.#+,-()?/:;<=>@[]{|}^`~"""
  private final val Special = P(CharIn(SpecialChar))

  private[parser] final val ReverseSolidus = P("\\")

  private[parser] final val Apostrophe = P("'")

  private[parser] final val LatinCodePoint = P(Space | Digit | Lower | Upper | Special | ReverseSolidus | Apostrophe)

  private[parser] final val HighCodePoint = P(CharPred(_ > '\u0080'))
}

trait EnhancedBasicAlphabetDefinition extends BasicAlphabetDefinition {
  private[parser] final val UpperOrLowerChar = UpperChar ++ LowerChar
  private[parser] final val UpperOrLower = P(CharIn(UpperOrLowerChar))

  private[parser] final val UpperOrDigits = P(CharsWhileIn(('0' to '9') ++ ('A' to 'Z'), 1))

  private[parser] final val UpperOrLowerOrDigits = P(CharsWhileIn(LowerChar ++ UpperChar ++ DigitChar, 1))

  private[parser] final val InstanceName = P(Upper ~ UpperOrDigits.?)

  private[parser] final val Name = P(Upper ~ UpperOrDigits.?)

  private[parser] final val Digits1 = P(CharsWhileIn(DigitChar, 1))
  private[parser] final val Digits0 = P(Digits1.?)

  private[parser] final val StringChar = P(CharsWhileIn(SpecialChar ++ DigitChar ++ SpaceChar ++ LowerChar ++ UpperChar))
}