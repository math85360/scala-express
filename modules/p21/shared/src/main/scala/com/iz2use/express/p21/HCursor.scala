package com.iz2use.express.p21

import cursor._

abstract class HCursor(lastCursor: HCursor, lastOp: CursorOp) extends ACursor(lastCursor, lastOp) {
  def value: Step

  def replace(newValue: Step, cursor: HCursor, op: CursorOp): HCursor
  /*final def downObject: ACursor = value match {
    case StepObject(name, values) =>
      new ObjectCursor(values, 0, this, false)(this, CursorOp.DownObject)
    case _ => fail(CursorOp.DownObject)
  }*/

  final def succeeded: Boolean = true

  final def downArray: ACursor = value match {
    case StepArray(values) =>
      new ArrayCursor(values, 0, this, false)(this, CursorOp.DownArray)
    case _ => fail(CursorOp.DownArray)
  }

  protected[this] final def fail(op: CursorOp): ACursor = new FailedCursor(this, op)
}