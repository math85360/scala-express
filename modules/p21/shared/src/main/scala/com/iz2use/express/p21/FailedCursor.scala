package com.iz2use.express.p21

final class FailedCursor(lastCursor: HCursor, lastOp: CursorOp) extends ACursor(lastCursor, lastOp) {
  def downArray: ACursor = this
  def delete: ACursor = this
  def succeeded: Boolean = false
  def right: ACursor = this
}