package com.iz2use.express.parser

import com.iz2use.express.ast
import scala.language.postfixOps
import IgnoringParts._
import fastparse.noApi._
import scala.annotation.tailrec

trait Literal extends BasicAlphabetDefinition with KeywordDefinition {
  private[parser] val binary_literal: P[ast.BinaryLiteral] = P(("%" ~ bit.rep(1).!.map({ v =>
    val len = v.length
    @tailrec
    def loop(index: Int, acc: List[Int]): collection.BitSet =
      if (index < 0) collection.BitSet(acc: _*)
      else loop(index - 1, v.charAt(index) match {
        case 0 => acc
        case 1 => (len - index - 1) :: acc
      })
    ast.BinaryLiteral(loop(len - 1, Nil))
  })))
  private[parser] val encoded_string_literal: P[ast.StringLiteral] = P(("\"" ~ encoded_character.rep(1).! ~ "\"")
    .map(ast.StringLiteral))
  private val integer_literal: P[ast.IntegerLiteral] = P(digits
    .!.map(ast.IntegerLiteral))
  private[parser] val literal: P[ast.Literal] = P((binary_literal | logical_literal | real_literal | string_literal))
  private val logical_literal: P[ast.LogicalLiteral] = P((FALSE.to(Some(false)) | TRUE.to(Some(true)) | UNKNOWN.to(None))
    .map(ast.LogicalLiteral))
  private[parser] val real_literal: P[ast.NumberLiteral] = P((digits ~ "." ~ digits.? ~ ("e" ~ sign.? ~ digits).?).!.map(ast.RealLiteral) | integer_literal)
  private[parser] val string_literal: P[ast.StringLiteral] = P(simple_string_literal | encoded_string_literal)
  private[parser] val simple_string_literal: P[ast.StringLiteral] = P(("'" ~ (("''") | not_quote | " " | "\b" | "\t" | "\n" | "\u000b" | "\f" | "\r").rep.! ~ "'")
    .map(ast.StringLiteral))
}