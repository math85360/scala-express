package com.iz2use.express.p11.parser

import scala.language.postfixOps
import fastparse.all._

object MkKeyword {
  import BasicAlphabetDefinition._
  def apply(keyword: String)(implicit ev: sourcecode.Name) = 
    P((IgnoreCase(keyword)~ !simple_id_tail).opaque(keyword))
}
trait KeywordDefinition {
  import BasicAlphabetDefinition._
  
  private[parser] val ABS = MkKeyword("ABS")
  private[parser] val ABSTRACT = MkKeyword("ABSTRACT")
  private[parser] val ACOS = MkKeyword("ACOS")
  private[parser] val AGGREGATE = MkKeyword("AGGREGATE")
  private[parser] val ALIAS = MkKeyword("ALIAS")
  private[parser] val AND = MkKeyword("AND")
  private[parser] val ANDOR = MkKeyword("ANDOR")
  private[parser] val ARRAY = MkKeyword("ARRAY")
  private[parser] val AS = MkKeyword("AS")
  private[parser] val ASIN = MkKeyword("ASIN")
  private[parser] val ATAN = MkKeyword("ATAN")
  private[parser] val BAG = MkKeyword("BAG")
  private[parser] val BASED_ON = MkKeyword("BASED_ON")
  private[parser] val BEGIN = MkKeyword("BEGIN")
  private[parser] val BINARY = MkKeyword("BINARY")
  private[parser] val BLENGTH = MkKeyword("BLENGTH")
  private[parser] val BOOLEAN = MkKeyword("BOOLEAN")
  private[parser] val BY = MkKeyword("BY")
  private[parser] val CASE = MkKeyword("CASE")
  private[parser] val CONSTANT = MkKeyword("CONSTANT")
  private[parser] val CONST_E = MkKeyword("CONST_E")
  private[parser] val COS = MkKeyword("COS")
  private[parser] val DERIVE = MkKeyword("DERIVE")
  private[parser] val DIV = MkKeyword("DIV")
  private[parser] val ELSE = MkKeyword("ELSE")
  private[parser] val END = MkKeyword("END")
  private[parser] val END_ALIAS = MkKeyword("END_ALIAS")
  private[parser] val END_CASE = MkKeyword("END_CASE")
  private[parser] val END_CONSTANT = MkKeyword("END_CONSTANT")
  private[parser] val END_ENTITY = MkKeyword("END_ENTITY")
  private[parser] val END_FUNCTION = MkKeyword("END_FUNCTION")
  private[parser] val END_IF = MkKeyword("END_IF")
  private[parser] val END_LOCAL = MkKeyword("END_LOCAL")
  private[parser] val END_PROCEDURE = MkKeyword("END_PROCEDURE")
  private[parser] val END_REPEAT = MkKeyword("END_REPEAT")
  private[parser] val END_RULE = MkKeyword("END_RULE")
  private[parser] val END_SCHEMA = MkKeyword("END_SCHEMA")
  private[parser] val END_SUBTYPE_CONSTRAINT = MkKeyword("END_SUBTYPE_CONSTRAINT")
  private[parser] val END_TYPE = MkKeyword("END_TYPE")
  private[parser] val ENTITY = MkKeyword("ENTITY")
  private[parser] val ENUMERATION = MkKeyword("ENUMERATION")
  private[parser] val ESCAPE = MkKeyword("ESCAPE")
  private[parser] val EXISTS = MkKeyword("EXISTS")
  private[parser] val EXTENSIBLE = MkKeyword("EXTENSIBLE")
  private[parser] val EXP = MkKeyword("EXP")
  private[parser] val FALSE = MkKeyword("FALSE")
  private[parser] val FIXED = MkKeyword("FIXED")
  private[parser] val FOR = MkKeyword("FOR")
  private[parser] val FORMAT = MkKeyword("FORMAT")
  private[parser] val FROM = MkKeyword("FROM")
  private[parser] val FUNCTION = MkKeyword("FUNCTION")
  private[parser] val GENERIC = MkKeyword("GENERIC")
  private[parser] val GENERIC_ENTITY = MkKeyword("GENERIC_ENTITY")
  private[parser] val HIBOUND = MkKeyword("HIBOUND")
  private[parser] val HIINDEX = MkKeyword("HIINDEX")
  private[parser] val IF = MkKeyword("IF")
  private[parser] val IN = MkKeyword("IN")
  private[parser] val INSERT = MkKeyword("INSERT")
  private[parser] val INTEGER = MkKeyword("INTEGER")
  private[parser] val INVERSE = MkKeyword("INVERSE")
  private[parser] val LENGTH = MkKeyword("LENGTH")
  private[parser] val LIKE = MkKeyword("LIKE")
  private[parser] val LIST = MkKeyword("LIST")
  private[parser] val LOBOUND = MkKeyword("LOBOUND")
  private[parser] val LOCAL = MkKeyword("LOCAL")
  private[parser] val LOG = MkKeyword("LOG")
  private[parser] val LOG10 = MkKeyword("LOG10")
  private[parser] val LOG2 = MkKeyword("LOG2")
  private[parser] val LOGICAL = MkKeyword("LOGICAL")
  private[parser] val LOINDEX = MkKeyword("LOINDEX")
  private[parser] val MOD = MkKeyword("MOD")
  private[parser] val NOT = MkKeyword("NOT")
  private[parser] val NUMBER = MkKeyword("NUMBER")
  private[parser] val NVL = MkKeyword("NVL")
  private[parser] val ODD = MkKeyword("ODD")
  private[parser] val OF = MkKeyword("OF")
  private[parser] val ONEOF = MkKeyword("ONEOF")
  private[parser] val OPTIONAL = MkKeyword("OPTIONAL")
  private[parser] val OR = MkKeyword("OR")
  private[parser] val OTHERWISE = MkKeyword("OTHERWISE")
  private[parser] val PI = MkKeyword("PI")
  private[parser] val PROCEDURE = MkKeyword("PROCEDURE")
  private[parser] val QUERY = MkKeyword("QUERY")
  private[parser] val REAL = MkKeyword("REAL")
  private[parser] val REFERENCE = MkKeyword("REFERENCE")
  private[parser] val REMOVE = MkKeyword("REMOVE")
  private[parser] val RENAMED = MkKeyword("RENAMED")
  private[parser] val REPEAT = MkKeyword("REPEAT")
  private[parser] val RETURN = MkKeyword("RETURN")
  private[parser] val ROLESOF = MkKeyword("ROLESOF")
  private[parser] val RULE = MkKeyword("RULE")
  private[parser] val SCHEMA = MkKeyword("SCHEMA")
  private[parser] val SELECT = MkKeyword("SELECT")
  private[parser] val SELF = MkKeyword("SELF")
  private[parser] val SET = MkKeyword("SET")
  private[parser] val SIN = MkKeyword("SIN")
  private[parser] val SIZEOF = MkKeyword("SIZEOF")
  private[parser] val SKIP = MkKeyword("SKIP")
  private[parser] val SQRT = MkKeyword("SQRT")
  private[parser] val STRING = MkKeyword("STRING")
  private[parser] val SUBTYPE = MkKeyword("SUBTYPE")
  private[parser] val SUBTYPE_CONSTRAINT = MkKeyword("SUBTYPE_CONSTRAINT")
  private[parser] val SUPERTYPE = MkKeyword("SUPERTYPE")
  private[parser] val TAN = MkKeyword("TAN")
  private[parser] val THEN = MkKeyword("THEN")
  private[parser] val TO = MkKeyword("TO")
  private[parser] val TOTAL_OVER = MkKeyword("TOTAL_OVER")
  private[parser] val TRUE = MkKeyword("TRUE")
  private[parser] val TYPE = MkKeyword("TYPE")
  private[parser] val TYPEOF = MkKeyword("TYPEOF")
  private[parser] val UNIQUE = MkKeyword("UNIQUE")
  private[parser] val UNKNOWN = MkKeyword("UNKNOWN")
  private[parser] val UNTIL = MkKeyword("UNTIL")
  private[parser] val USE = MkKeyword("USE")
  private[parser] val USEDIN = MkKeyword("USEDIN")
  private[parser] val VALUE = MkKeyword("VALUE")
  private[parser] val VALUE_IN = MkKeyword("VALUE_IN")
  private[parser] val VALUE_UNIQUE = MkKeyword("VALUE_UNIQUE")
  private[parser] val VAR = MkKeyword("VAR")
  private[parser] val WHERE = MkKeyword("WHERE")
  private[parser] val WHILE = MkKeyword("WHILE")
  private[parser] val WITH = MkKeyword("WITH")
  private[parser] val XOR = MkKeyword("XOR")
}