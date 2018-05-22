package com.iz2use.express.p21.cursor

import com.iz2use.express.p21.{ ACursor, CursorOp, HCursor, Step }

private[p21] final class ArrayCursor(values: Vector[Step], index: Int, parent: HCursor, changed: Boolean)(
    lastCursor: HCursor,
    lastOp:     CursorOp) extends HCursor(lastCursor, lastOp) {
  def value: Step = values(index)

  private[this] def valuesExcept: Vector[Step] = values.take(index) ++ values.drop(index + 1)

  def replace(newValue: Step, cursor: HCursor, op: CursorOp): HCursor =
    new ArrayCursor(values.updated(index, newValue), index, parent, true)(cursor, op)

  def delete: ACursor = parent.replace(Step.fromValues(valuesExcept), this, CursorOp.DeleteGoParent)

  def right: ACursor = if (index == values.size - 1) fail(CursorOp.MoveRight) else {
    new ArrayCursor(values, index + 1, parent, changed)(this, CursorOp.MoveRight)
  }
}