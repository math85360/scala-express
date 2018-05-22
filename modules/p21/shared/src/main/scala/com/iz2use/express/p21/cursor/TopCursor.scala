package com.iz2use.express.p21.cursor

import com.iz2use.express.p21.{ ACursor, CursorOp, HCursor, Step }

private[p21] final class TopCursor(val value: Step)(
    lastCursor: HCursor,
    lastOp:     CursorOp) extends HCursor(lastCursor, lastOp) {
  def replace(newValue: Step, cursor: HCursor, op: CursorOp): HCursor = new TopCursor(newValue)(cursor, op)
  def delete: ACursor = fail(CursorOp.DeleteGoParent)
  def right: ACursor = fail(CursorOp.MoveRight)
}