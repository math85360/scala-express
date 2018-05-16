package com.iz2use.express.p21

sealed abstract class DecodingFailure(val message: String) extends Error {
  def history: List[CursorOp]
}

final object DecodingFailure {
  def apply(message: String, ops: => List[CursorOp]): DecodingFailure = new DecodingFailure(message) {
    final lazy val history: List[CursorOp] = ops
  }
}
