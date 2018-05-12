package com.iz2use.express.p21.parser

import scala.language.postfixOps
import IgnoringParts._
import fastparse.noApi._

trait ExchangeFileStructure extends StringStructure {

  private val ExchangeFile = P("ISO-10303-21;" ~/
    HeaderSection ~/ AnchorSection.? ~
    ReferenceSection.? ~ DataSection.rep ~
    "END-ISO-10303-21;" ~/ SignatureSection.rep ~/)

  private val HeaderSection = P("HEADER;" ~/
    HeaderEntity ~ HeaderEntity ~ HeaderEntity ~
    HeaderEntityList.? ~
    "ENDSEC;")

  private val HeaderEntityList = P(HeaderEntity.rep(1))

  private val HeaderEntity = P(Keyword ~ "(" ~ ParameterList ~ ")" ~ ";"~/)

  private val ParameterList = P(Parameter.rep(sep = ","))

  private val Parameter: P[Unit] = P(TypedParameter |
    UntypedParameter | OmmittedParameter)

  private val TypedParameter = P(Keyword ~ "(" ~/ Parameter ~ ")" ~/)
  private val UntypedParameter = P("$" | Integer | Real | String | RhsOccurrenceName | Enumeration | Binary | List)
  private val OmmittedParameter = P("*" ~/)
  private val List = P("(" ~/ Parameter.rep(sep = ",") ~ ")" ~/)

  private val AnchorSection = P("ANCHOR;" ~/ AnchorList ~ "ENDSEC;" ~/)
  private val AnchorList = P(Anchor.rep(1))
  private val Anchor = P(AnchorName ~ "=" ~ AnchorItem ~ AnchorTag.rep ~ ";" ~/)
  private val AnchorItem: P[Unit] = P("$" | Integer | Real | String | Enumeration | Binary
    | RhsOccurrenceName | Resource | AnchorItemList)
  private val AnchorItemList = P("(" ~/ AnchorItem.rep(sep = ",") ~ ")" ~/)
  private val AnchorTag = P("{" ~/ TagName ~ ":" ~ AnchorItem ~ "}" ~/)

  private val ReferenceSection = P("REFERENCE;" ~/ ReferenceList ~ "ENDSEC;"~/)
  private val ReferenceList = P(Reference.rep(1))
  private val Reference = P(LhsOccurrenceName ~ "=" ~ Resource ~ ";")

  private val DataSection = P("DATA" ~ ("(" ~ ParameterList ~ ")").? ~ ";" ~
    EntityInstanceList ~ "ENDSEC;")
  private val EntityInstanceList = P(EntityInstance.rep(1))
  private val EntityInstance = P(SimpleEntityInstance | ComplexEntityInstance)
  private val SimpleEntityInstance = P(EntityInstanceName ~ "=" ~ SimpleRecord ~ ";")
  private val ComplexEntityInstance = P(EntityInstanceName ~ "=" ~ SubSuperRecord ~ ";")
  private val SimpleRecord = P(Keyword ~ "(" ~ ParameterList.? ~ ")")
  private val SubSuperRecord = P("(" ~/ SimpleRecordList ~ ")" ~/)
  private val SimpleRecordList = P(SimpleRecord.rep(1))

  private val SignatureSection = P("SIGNATURE" ~/ SignatureContent ~ "ENDSEC;"~/)
}