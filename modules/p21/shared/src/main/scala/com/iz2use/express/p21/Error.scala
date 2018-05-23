package com.iz2use.express.p21

sealed abstract class DecodingFailure(val message: String) extends Error {
  def history: List[CursorOp]
  override def toString: String = s"""DecodingFailure($message, ${history.mkString("\n  ", "\n  ", "\n  ")})"""
}

final object DecodingFailure {
  def apply(message: String, ops: => List[CursorOp]): DecodingFailure = {
    val r = new DecodingFailure(message) {
      final lazy val history: List[CursorOp] = ops
    }
    throw r
    r
  }
}
