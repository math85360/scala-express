package com.iz2use.express.p21

sealed abstract class DecodingFailure(val message: String) extends Error {
  def history: Step
}

final object DecodingFailure {
  def apply(message: String, ops: => Step): DecodingFailure = new DecodingFailure(message) {
    final lazy val history: Step = ops
  }
}
