package com.iz2use.express.parser

import scala.language.postfixOps
import fastparse.all._

trait KeywordDefinition {
  private[parser] val ABS = P("ABS" ~/)
  private[parser] val ABSTRACT = P("ABSTRACT" ~/)
  private[parser] val ACOS = P("ACOS" ~/)
  private[parser] val AGGREGATE = P("AGGREGATE" ~/)
  private[parser] val ALIAS = P("ALIAS" ~/)
  private[parser] val AND = P("AND" ~/)
  private[parser] val ANDOR = P("ANDOR" ~/)
  private[parser] val ARRAY = P("ARRAY" ~/)
  private[parser] val AS = P("AS" ~/)
  private[parser] val ASIN = P("ASIN" ~/)
  private[parser] val ATAN = P("ATAN" ~/)
  private[parser] val BAG = P("BAG" ~/)
  private[parser] val BASED_ON = P("BASED_ON" ~/)
  private[parser] val BEGIN = P("BEGIN" ~/)
  private[parser] val BINARY = P("BINARY" ~/)
  private[parser] val BLENGTH = P("BLENGTH" ~/)
  private[parser] val BOOLEAN = P("BOOLEAN" ~/)
  private[parser] val BY = P("BY" ~/)
  private[parser] val CASE = P("CASE" ~/)
  private[parser] val CONSTANT = P("CONSTANT" ~/)
  private[parser] val CONST_E = P("CONST_E" ~/)
  private[parser] val COS = P("COS" ~/)
  private[parser] val DERIVE = P("DERIVE" ~/)
  private[parser] val DIV = P("DIV" ~/)
  private[parser] val ELSE = P("ELSE" ~/)
  private[parser] val END = P("END" ~/)
  private[parser] val END_ALIAS = P("END_ALIAS" ~/)
  private[parser] val END_CASE = P("END_CASE" ~/)
  private[parser] val END_CONSTANT = P("END_CONSTANT" ~/)
  private[parser] val END_ENTITY = P("END_ENTITY" ~/)
  private[parser] val END_FUNCTION = P("END_FUNCTION" ~/)
  private[parser] val END_IF = P("END_IF" ~/)
  private[parser] val END_LOCAL = P("END_LOCAL" ~/)
  private[parser] val END_PROCEDURE = P("END_PROCEDURE" ~/)
  private[parser] val END_REPEAT = P("END_REPEAT" ~/)
  private[parser] val END_RULE = P("END_RULE" ~/)
  private[parser] val END_SCHEMA = P("END_SCHEMA" ~/)
  private[parser] val END_SUBTYPE_CONSTRAINT = P("END_SUBTYPE_CONSTRAINT" ~/)
  private[parser] val END_TYPE = P("END_TYPE" ~/)
  private[parser] val ENTITY = P("ENTITY" ~/)
  private[parser] val ENUMERATION = P("ENUMERATION" ~/)
  private[parser] val ESCAPE = P("ESCAPE" ~/)
  private[parser] val EXISTS = P("EXISTS" ~/)
  private[parser] val EXTENSIBLE = P("EXTENSIBLE" ~/)
  private[parser] val EXP = P("EXP" ~/)
  private[parser] val FALSE = P("FALSE" ~/)
  private[parser] val FIXED = P("FIXED" ~/)
  private[parser] val FOR = P("FOR" ~/)
  private[parser] val FORMAT = P("FORMAT" ~/)
  private[parser] val FROM = P("FROM" ~/)
  private[parser] val FUNCTION = P("FUNCTION" ~/)
  private[parser] val GENERIC = P("GENERIC" ~/)
  private[parser] val GENERIC_ENTITY = P("GENERIC_ENTITY" ~/)
  private[parser] val HIBOUND = P("HIBOUND" ~/)
  private[parser] val HIINDEX = P("HIINDEX" ~/)
  private[parser] val IF = P("IF" ~/)
  private[parser] val IN = P("IN" ~/)
  private[parser] val INSERT = P("INSERT" ~/)
  private[parser] val INTEGER = P("INTEGER" ~/)
  private[parser] val INVERSE = P("INVERSE" ~/)
  private[parser] val LENGTH = P("LENGTH" ~/)
  private[parser] val LIKE = P("LIKE" ~/)
  private[parser] val LIST = P("LIST" ~/)
  private[parser] val LOBOUND = P("LOBOUND" ~/)
  private[parser] val LOCAL = P("LOCAL" ~/)
  private[parser] val LOG = P("LOG" ~/)
  private[parser] val LOG10 = P("LOG10" ~/)
  private[parser] val LOG2 = P("LOG2" ~/)
  private[parser] val LOGICAL = P("LOGICAL" ~/)
  private[parser] val LOINDEX = P("LOINDEX" ~/)
  private[parser] val MOD = P("MOD" ~/)
  private[parser] val NOT = P("NOT" ~/)
  private[parser] val NUMBER = P("NUMBER" ~/)
  private[parser] val NVL = P("NVL" ~/)
  private[parser] val ODD = P("ODD" ~/)
  private[parser] val OF = P("OF" ~/)
  private[parser] val ONEOF = P("ONEOF" ~/)
  private[parser] val OPTIONAL = P("OPTIONAL" ~/)
  private[parser] val OR = P("OR" ~/)
  private[parser] val OTHERWISE = P("OTHERWISE" ~/)
  private[parser] val PI = P("PI" ~/)
  private[parser] val PROCEDURE = P("PROCEDURE" ~/)
  private[parser] val QUERY = P("QUERY" ~/)
  private[parser] val REAL = P("REAL" ~/)
  private[parser] val REFERENCE = P("REFERENCE" ~/)
  private[parser] val REMOVE = P("REMOVE" ~/)
  private[parser] val RENAMED = P("RENAMED" ~/)
  private[parser] val REPEAT = P("REPEAT" ~/)
  private[parser] val RETURN = P("RETURN" ~/)
  private[parser] val ROLESOF = P("ROLESOF" ~/)
  private[parser] val RULE = P("RULE" ~/)
  private[parser] val SCHEMA = P("SCHEMA" ~/)
  private[parser] val SELECT = P("SELECT" ~/)
  private[parser] val SELF = P("SELF" ~/)
  private[parser] val SET = P("SET" ~/)
  private[parser] val SIN = P("SIN" ~/)
  private[parser] val SIZEOF = P("SIZEOF" ~/)
  private[parser] val SKIP = P("SKIP" ~/)
  private[parser] val SQRT = P("SQRT" ~/)
  private[parser] val STRING = P("STRING" ~/)
  private[parser] val SUBTYPE = P("SUBTYPE" ~/)
  private[parser] val SUBTYPE_CONSTRAINT = P("SUBTYPE_CONSTRAINT" ~/)
  private[parser] val SUPERTYPE = P("SUPERTYPE" ~/)
  private[parser] val TAN = P("TAN" ~/)
  private[parser] val THEN = P("THEN" ~/)
  private[parser] val TO = P("TO" ~/)
  private[parser] val TOTAL_OVER = P("TOTAL_OVER" ~/)
  private[parser] val TRUE = P("TRUE" ~/)
  private[parser] val TYPE = P("TYPE" ~/)
  private[parser] val TYPEOF = P("TYPEOF" ~/)
  private[parser] val UNIQUE = P("UNIQUE" ~/)
  private[parser] val UNKNOWN = P("UNKNOWN" ~/)
  private[parser] val UNTIL = P("UNTIL" ~/)
  private[parser] val USE = P("USE" ~/)
  private[parser] val USEDIN = P("USEDIN" ~/)
  private[parser] val VALUE = P("VALUE" ~/)
  private[parser] val VALUE_IN = P("VALUE_IN" ~/)
  private[parser] val VALUE_UNIQUE = P("VALUE_UNIQUE" ~/)
  private[parser] val VAR = P("VAR" ~/)
  private[parser] val WHERE = P("WHERE" ~/)
  private[parser] val WHILE = P("WHILE" ~/)
  private[parser] val WITH = P("WITH" ~/)
  private[parser] val XOR = P("XOR" ~/)
}